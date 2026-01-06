use ansi_term::{
    ANSIString, ANSIStrings,
    Colour::{Fixed, RGB},
    Style,
};
use std::collections::{BTreeMap, HashMap};
use zellij_tile::prelude::actions::Action;
use zellij_tile::prelude::actions::SearchDirection;
use zellij_tile::prelude::*;
use zellij_tile_utils::palette_match;

#[derive(Default)]
struct State {
    initialized: bool,
    pipe_name: String,
    mode_info: ModeInfo,
    base_mode_is_locked: bool,
    max_length: usize,
    overflow_str: String,
    hide_in_base_mode: bool,
    // Custom color configuration
    color_config: ColorConfig,
    // Custom label format configuration
    label_format_config: LabelFormatConfig,
    // Custom label text overrides
    label_text_overrides: HashMap<String, String>,
    // Custom modifier format configuration
    modifier_format_config: ModifierFormatConfig,
}

register_plugin!(State);

const TO_NORMAL: Action = Action::SwitchToMode(InputMode::Normal);

const PLUGIN_SESSION_MANAGER: &str = "session-manager";
const PLUGIN_CONFIGURATION: &str = "configuration";
const PLUGIN_MANAGER: &str = "plugin-manager";
const PLUGIN_ABOUT: &str = "zellij:about";

const KEY_PATTERNS_NO_SEPARATOR: &[&str] = &["HJKL", "hjkl", "←↓↑→", "←→", "↓↑", "[]"];

const DEFAULT_MAX_LENGTH: usize = 0;
const DEFAULT_OVERFLOW_STR: &str = "...";
const DEFAULT_PIPE_NAME: &str = "zjstatus_hints";

type ActionLabel = (Action, &'static str);
type ActionSequenceLabel = (&'static [Action], &'static str);

/// Parse a hex color string (e.g., "#RRGGBB" or "RRGGBB") into an ansi_term::Colour
fn parse_hex_color(s: &str) -> Option<ansi_term::Colour> {
    let hex = s.trim_start_matches('#');
    if hex.len() != 6 {
        return None;
    }
    let r = u8::from_str_radix(&hex[0..2], 16).ok()?;
    let g = u8::from_str_radix(&hex[2..4], 16).ok()?;
    let b = u8::from_str_radix(&hex[4..6], 16).ok()?;
    Some(RGB(r, g, b))
}

/// Parse per-label text overrides from configuration
///
/// Format: "label_text" for label text replacements
/// Example: "pane_label_text" = "p" replaces "pane" with "p"
///
/// For labels with spaces, use underscores: "split_right_label_text"
fn parse_label_text_overrides(config: &BTreeMap<String, String>) -> HashMap<String, String> {
    let mut overrides = HashMap::new();
    const LABEL_TEXT_SUFFIX: &str = "_label_text";

    for (key, value) in config.iter() {
        if key.ends_with(LABEL_TEXT_SUFFIX) {
            let label_name = key.trim_end_matches(LABEL_TEXT_SUFFIX);
            if label_name.is_empty() {
                continue;
            }

            // Convert underscores to spaces for multi-word labels
            let label_key = label_name.replace('_', " ");

            if !label_key.trim().is_empty() && !value.trim().is_empty() {
                overrides.insert(label_key, value.clone());
            }
        }
    }
    overrides
}

/// Get the display label text, with mode-specific and label-only override support
/// Priority: mode-specific (e.g., "pane.new") > label-only (e.g., "new") > default
fn get_label_text(overrides: &HashMap<String, String>, mode: Option<&str>, label: &str) -> String {
    // Try mode-specific override first (e.g., "pane.new")
    if let Some(m) = mode {
        let mode_specific_key = format!("{}.{}", m, label);
        if let Some(override_text) = overrides.get(&mode_specific_key) {
            return override_text.clone();
        }
    }

    // Fall back to label-only override (e.g., "new")
    overrides
        .get(label)
        .cloned()
        .unwrap_or_else(|| label.to_string())
}

/// Parse per-label color overrides from configuration
///
/// Supports two formats:
/// - Label-only: "select_key_bg" -> applies to "select" label in all modes
/// - Mode-specific: "pane.select_key_bg" -> applies to "select" label only in pane mode
///
/// For labels with spaces (e.g., "split right"), use underscores: "split_right_key_bg"
/// Mode-specific keys use dots to separate mode from label: "pane.split_right_key_bg"
fn parse_label_overrides(config: &BTreeMap<String, String>) -> HashMap<String, LabelColors> {
    let mut overrides = HashMap::new();
    let color_suffixes = ["_key_fg", "_key_bg", "_label_fg", "_label_bg"];

    for (key, value) in config.iter() {
        for suffix in &color_suffixes {
            if key.ends_with(suffix) {
                let label_name = key.trim_end_matches(suffix);
                if label_name.is_empty() || label_name == "key" || label_name == "label" {
                    continue;
                }

                // Check if this is a mode-specific key (contains a dot)
                let label_key = if let Some(dot_pos) = label_name.find('.') {
                    // Mode-specific: "pane.select" or "pane.split_right" -> "pane.select" or "pane.split right"
                    let mode = &label_name[..dot_pos];
                    let label = &label_name[dot_pos + 1..];
                    if mode.is_empty() || label.is_empty() {
                        continue;
                    }
                    format!("{}.{}", mode, label.replace('_', " "))
                } else {
                    // Label-only: "select" or "split_right" -> "select" or "split right"
                    label_name.replace('_', " ")
                };

                // Skip whitespace-only labels (e.g., "__key_bg" -> "  ")
                if label_key.trim().is_empty() || label_key.trim() == "." {
                    continue;
                }
                // Only insert entry if color parses successfully
                if let Some(color) = parse_hex_color(value) {
                    let entry = overrides.entry(label_key).or_insert_with(LabelColors::default);
                    match *suffix {
                        "_key_fg" => entry.key_fg = Some(color),
                        "_key_bg" => entry.key_bg = Some(color),
                        "_label_fg" => entry.label_fg = Some(color),
                        "_label_bg" => entry.label_bg = Some(color),
                        _ => {}
                    }
                }
                break;
            }
        }
    }
    overrides
}

/// Per-label color configuration
#[derive(Default, Clone, Copy)]
struct LabelColors {
    key_fg: Option<ansi_term::Colour>,
    key_bg: Option<ansi_term::Colour>,
    label_fg: Option<ansi_term::Colour>,
    label_bg: Option<ansi_term::Colour>,
}

/// Custom color configuration for hints styling
#[derive(Default, Clone)]
struct ColorConfig {
    defaults: LabelColors,
    overrides: HashMap<String, LabelColors>,
}

/// Label format template for custom keybind display
#[derive(Clone)]
struct LabelFormat {
    template: String,
}

impl Default for LabelFormat {
    fn default() -> Self {
        LabelFormat {
            template: "{combo}".to_string(),
        }
    }
}

/// Custom label format configuration for keybind display
#[derive(Default, Clone)]
struct LabelFormatConfig {
    defaults: LabelFormat,
    overrides: HashMap<String, LabelFormat>,
}

/// Preset options for modifier formatting
#[derive(Clone, Copy, PartialEq, Debug)]
enum ModifierPreset {
    /// Full modifier names: "Ctrl", "Alt", "Shift", "Super"
    Full,
    /// Brief modifier names: "C", "A", "S", "M" (using M for Super as a common convention)
    Brief,
    /// Stripped: displays only the key without any modifier prefix (e.g., just "q" instead of "Ctrl + q")
    Stripped,
}

impl Default for ModifierPreset {
    fn default() -> Self {
        ModifierPreset::Full
    }
}

/// Custom modifier format configuration
///
/// Supports two mutually exclusive modes:
/// 1. Preset mode: Use a predefined format (full, brief, stripped)
/// 2. Custom mode: Define custom formats for each modifier
#[derive(Clone)]
struct ModifierFormatConfig {
    /// Custom format for Ctrl modifier (e.g., "^", "C", "Ctrl")
    ctrl_format: String,
    /// Custom format for Alt modifier (e.g., "A", "Alt", "M")
    alt_format: String,
    /// Custom format for Shift modifier (e.g., "S", "Shift", "⇧")
    shift_format: String,
    /// Custom format for Super modifier (e.g., "M", "Super", "⌘")
    super_format: String,
    /// Separator between modifiers (e.g., "-", "+", "")
    modifier_separator: String,
    /// Separator between modifiers and key (e.g., " + ", "-", "")
    key_separator: String,
    /// Custom format order template (e.g., "{mods}{sep}{key}" or "{key}{sep}{mods}")
    format_order: String,
}

impl Default for ModifierFormatConfig {
    fn default() -> Self {
        ModifierFormatConfig {
            ctrl_format: "Ctrl".to_string(),
            alt_format: "Alt".to_string(),
            shift_format: "Shift".to_string(),
            super_format: "Super".to_string(),
            modifier_separator: "-".to_string(),
            key_separator: " + ".to_string(),
            format_order: "{mods}{sep}{key}".to_string(),
        }
    }
}

impl ModifierFormatConfig {
    /// Create a configuration from a preset
    fn from_preset(preset: ModifierPreset) -> Self {
        match preset {
            ModifierPreset::Full => ModifierFormatConfig {
                ctrl_format: "Ctrl".to_string(),
                alt_format: "Alt".to_string(),
                shift_format: "Shift".to_string(),
                super_format: "Super".to_string(),
                modifier_separator: "-".to_string(),
                key_separator: " + ".to_string(),
                format_order: "{mods}{sep}{key}".to_string(),
            },
            ModifierPreset::Brief => ModifierFormatConfig {
                ctrl_format: "C".to_string(),
                alt_format: "A".to_string(),
                shift_format: "S".to_string(),
                super_format: "M".to_string(),
                modifier_separator: "-".to_string(),
                key_separator: "-".to_string(),
                format_order: "{mods}{sep}{key}".to_string(),
            },
            ModifierPreset::Stripped => ModifierFormatConfig {
                ctrl_format: String::new(),
                alt_format: String::new(),
                shift_format: String::new(),
                super_format: String::new(),
                modifier_separator: String::new(),
                key_separator: String::new(),
                format_order: "{key}".to_string(),
            },
        }
    }
}

/// Parse modifier format preset from string
fn parse_modifier_preset(s: &str) -> Option<ModifierPreset> {
    match s.to_lowercase().as_str() {
        "full" => Some(ModifierPreset::Full),
        "brief" => Some(ModifierPreset::Brief),
        "stripped" => Some(ModifierPreset::Stripped),
        _ => None,
    }
}

/// Parse modifier format configuration from plugin configuration
fn parse_modifier_format_config(config: &BTreeMap<String, String>) -> ModifierFormatConfig {
    // Check if custom format options are specified (mutually exclusive with preset)
    let has_custom_format = config.contains_key("modifier_ctrl_format")
        || config.contains_key("modifier_alt_format")
        || config.contains_key("modifier_shift_format")
        || config.contains_key("modifier_super_format")
        || config.contains_key("modifier_separator")
        || config.contains_key("modifier_key_separator")
        || config.contains_key("modifier_format_order");

    if has_custom_format {
        // Use custom format options, starting from defaults
        let defaults = ModifierFormatConfig::default();
        ModifierFormatConfig {
            ctrl_format: config
                .get("modifier_ctrl_format")
                .cloned()
                .unwrap_or(defaults.ctrl_format),
            alt_format: config
                .get("modifier_alt_format")
                .cloned()
                .unwrap_or(defaults.alt_format),
            shift_format: config
                .get("modifier_shift_format")
                .cloned()
                .unwrap_or(defaults.shift_format),
            super_format: config
                .get("modifier_super_format")
                .cloned()
                .unwrap_or(defaults.super_format),
            modifier_separator: config
                .get("modifier_separator")
                .cloned()
                .unwrap_or(defaults.modifier_separator),
            key_separator: config
                .get("modifier_key_separator")
                .cloned()
                .unwrap_or(defaults.key_separator),
            format_order: config
                .get("modifier_format_order")
                .cloned()
                .unwrap_or(defaults.format_order),
        }
    } else {
        // Use preset (default to "full" if not specified or invalid)
        let preset = config
            .get("modifier_preset")
            .and_then(|s| parse_modifier_preset(s))
            .unwrap_or(ModifierPreset::Full);
        ModifierFormatConfig::from_preset(preset)
    }
}

/// Parse per-label format overrides from configuration
///
/// Supports two formats:
/// - Label-only: "select_key_format" -> applies to "select" label in all modes
/// - Mode-specific: "pane.select_key_format" -> applies to "select" label only in pane mode
///
/// For labels with spaces (e.g., "split right"), use underscores: "split_right_key_format"
/// Mode-specific keys use dots to separate mode from label: "pane.split_right_key_format"
fn parse_label_format_overrides(config: &BTreeMap<String, String>) -> HashMap<String, LabelFormat> {
    let mut overrides = HashMap::new();
    const FORMAT_SUFFIX: &str = "_key_format";

    for (key, value) in config.iter() {
        if key.ends_with(FORMAT_SUFFIX) {
            let label_name = key.trim_end_matches(FORMAT_SUFFIX);
            if label_name.is_empty() || label_name == "key" {
                continue;
            }

            // Check if this is a mode-specific key (contains a dot)
            let label_key = if let Some(dot_pos) = label_name.find('.') {
                // Mode-specific: "pane.select" or "pane.split_right" -> "pane.select" or "pane.split right"
                let mode = &label_name[..dot_pos];
                let label = &label_name[dot_pos + 1..];
                if mode.is_empty() || label.is_empty() {
                    continue;
                }
                format!("{}.{}", mode, label.replace('_', " "))
            } else {
                // Label-only: "select" or "split_right" -> "select" or "split right"
                label_name.replace('_', " ")
            };

            // Skip whitespace-only labels
            if label_key.trim().is_empty() || label_key.trim() == "." {
                continue;
            }

            // Validate template contains at least one of the required placeholders
            if value.contains("{keys}") || value.contains("{combo}") {
                overrides.insert(label_key, LabelFormat {
                    template: value.clone(),
                });
            }
        }
    }
    overrides
}

/// Get effective label format for a specific label, with mode-specific override support.
/// Lookup order: "{mode}.{label}" -> "{label}" -> defaults
fn get_format_for_label(config: &LabelFormatConfig, mode: Option<&str>, label: &str) -> String {
    // Try mode-specific override first (e.g., "pane.new")
    if let Some(m) = mode {
        let key = format!("{}.{}", m, label);
        if let Some(format) = config.overrides.get(&key) {
            return format.template.clone();
        }
    }

    // Fall back to label-only override (e.g., "new")
    if let Some(format) = config.overrides.get(label) {
        return format.template.clone();
    }

    // Use default
    config.defaults.template.clone()
}

/// Get effective colors for a specific label, with mode-specific override support.
/// Lookup order for each field: "{mode}.{label}" -> "{label}" -> defaults
/// Fields are merged independently, so mode-specific can override key_bg while
/// label-only provides label_fg.
fn get_colors_for_label(config: &ColorConfig, mode: Option<&str>, label: &str) -> LabelColors {
    // Try mode-specific override first (e.g., "pane.new")
    let mode_specific = mode.and_then(|m| {
        let key = format!("{}.{}", m, label);
        config.overrides.get(&key)
    });

    // Fall back to label-only override (e.g., "new")
    let label_only = config.overrides.get(label);

    // Merge all three levels: mode-specific -> label-only -> defaults
    // Each field falls through independently
    LabelColors {
        key_fg: mode_specific.and_then(|o| o.key_fg)
            .or(label_only.and_then(|o| o.key_fg))
            .or(config.defaults.key_fg),
        key_bg: mode_specific.and_then(|o| o.key_bg)
            .or(label_only.and_then(|o| o.key_bg))
            .or(config.defaults.key_bg),
        label_fg: mode_specific.and_then(|o| o.label_fg)
            .or(label_only.and_then(|o| o.label_fg))
            .or(config.defaults.label_fg),
        label_bg: mode_specific.and_then(|o| o.label_bg)
            .or(label_only.and_then(|o| o.label_bg))
            .or(config.defaults.label_bg),
    }
}

const NORMAL_MODE_ACTIONS: &[ActionLabel] = &[
    (Action::SwitchToMode(InputMode::Pane), "pane"),
    (Action::SwitchToMode(InputMode::Tab), "tab"),
    (Action::SwitchToMode(InputMode::Resize), "resize"),
    (Action::SwitchToMode(InputMode::Move), "move"),
    (Action::SwitchToMode(InputMode::Scroll), "scroll"),
    (Action::SwitchToMode(InputMode::Search), "search"),
    (Action::SwitchToMode(InputMode::Session), "session"),
    (Action::Quit, "quit"),
];

const PANE_MODE_ACTION_SEQUENCES: &[ActionSequenceLabel] = &[
    (&[Action::NewPane(None, None, false), TO_NORMAL], "new"),
    (&[Action::CloseFocus, TO_NORMAL], "close"),
    (&[Action::ToggleFocusFullscreen, TO_NORMAL], "fullscreen"),
    (&[Action::ToggleFloatingPanes, TO_NORMAL], "float"),
    (&[Action::TogglePaneEmbedOrFloating, TO_NORMAL], "embed"),
    (
        &[
            Action::NewPane(Some(Direction::Right), None, false),
            TO_NORMAL,
        ],
        "split right",
    ),
    (
        &[
            Action::NewPane(Some(Direction::Down), None, false),
            TO_NORMAL,
        ],
        "split down",
    ),
];

const TAB_MODE_ACTION_SEQUENCES: &[ActionSequenceLabel] = &[
    (
        &[
            Action::NewTab(None, vec![], None, None, None, true),
            TO_NORMAL,
        ],
        "new",
    ),
    (&[Action::CloseTab, TO_NORMAL], "close"),
    (&[Action::BreakPane, TO_NORMAL], "break pane"),
    (&[Action::ToggleActiveSyncTab, TO_NORMAL], "sync"),
];

fn get_common_modifiers(mut key_bindings: Vec<&KeyWithModifier>) -> Vec<KeyModifier> {
    if key_bindings.is_empty() {
        return vec![];
    }
    let mut common_modifiers = key_bindings.pop().unwrap().key_modifiers.clone();
    for key in key_bindings {
        common_modifiers = common_modifiers
            .intersection(&key.key_modifiers)
            .cloned()
            .collect();
    }
    common_modifiers.into_iter().collect()
}

impl ZellijPlugin for State {
    fn load(&mut self, configuration: BTreeMap<String, String>) {
        self.initialized = false;

        // TODO: configuration validation
        self.max_length = configuration
            .get("max_length")
            .and_then(|s| s.parse().ok())
            .unwrap_or(DEFAULT_MAX_LENGTH);
        self.overflow_str = configuration
            .get("overflow_str")
            .cloned()
            .unwrap_or_else(|| DEFAULT_OVERFLOW_STR.to_string());
        self.pipe_name = configuration
            .get("pipe_name")
            .cloned()
            .unwrap_or_else(|| DEFAULT_PIPE_NAME.to_string());
        self.hide_in_base_mode = configuration
            .get("hide_in_base_mode")
            .map(|s| s.to_lowercase().parse::<bool>().unwrap_or(false))
            .unwrap_or(false);

        // Parse custom color configuration
        self.color_config = ColorConfig {
            defaults: LabelColors {
                key_fg: configuration.get("key_fg").and_then(|s| parse_hex_color(s)),
                key_bg: configuration.get("key_bg").and_then(|s| parse_hex_color(s)),
                label_fg: configuration.get("label_fg").and_then(|s| parse_hex_color(s)),
                label_bg: configuration.get("label_bg").and_then(|s| parse_hex_color(s)),
            },
            overrides: parse_label_overrides(&configuration),
        };

        // Parse custom label format configuration
        let key_format = configuration
            .get("key_format")
            .cloned()
            .unwrap_or_else(|| "{combo}".to_string());

        // Validate that key_format contains at least one required placeholder
        let validated_format = if key_format.contains("{keys}") || key_format.contains("{combo}") {
            key_format
        } else {
            "{combo}".to_string()
        };

        self.label_format_config = LabelFormatConfig {
            defaults: LabelFormat {
                template: validated_format,
            },
            overrides: parse_label_format_overrides(&configuration),
        };

        // Parse custom label text overrides
        self.label_text_overrides = parse_label_text_overrides(&configuration);

        // Parse custom modifier format configuration
        self.modifier_format_config = parse_modifier_format_config(&configuration);

        request_permission(&[
            PermissionType::ReadApplicationState,
            PermissionType::MessageAndLaunchOtherPlugins,
        ]);

        set_selectable(false);
        subscribe(&[EventType::ModeUpdate, EventType::SessionUpdate]);
    }

    fn update(&mut self, event: Event) -> bool {
        let mut should_render = !self.initialized;
        if let Event::ModeUpdate(mode_info) = event {
            if self.mode_info != mode_info {
                should_render = true;
            }
            self.mode_info = mode_info;
            self.base_mode_is_locked = self.mode_info.base_mode == Some(InputMode::Locked);
        };
        should_render
    }

    fn render(&mut self, _rows: usize, _cols: usize) {
        let mode_info = &self.mode_info;
        let output = if !(self.hide_in_base_mode && Some(mode_info.mode) == mode_info.base_mode) {
            let keymap = get_keymap_for_mode(mode_info);
            let parts = render_hints_for_mode(mode_info.mode, &keymap, &mode_info.style.colors, &self.color_config, &self.label_format_config, &self.modifier_format_config, &self.label_text_overrides);

            let ansi_strings = ANSIStrings(&parts);
            let formatted = format!(" {}", ansi_strings);

            let visible_len = calculate_visible_length(&formatted);
            if self.max_length > 0 && visible_len > self.max_length {
                truncate_ansi_string(&formatted, &self.overflow_str, self.max_length)
            } else {
                formatted.to_string()
            }
        } else {
            String::new()
        };

        // HACK: Because we're not sure when zjstatus will be ready to receive messages,
        // we'll repeatedly send messages until the user has switched to a different mode,
        // at which point we'll assume that zjstatus has been initialized. The render function
        // does not seem to be called too frequently, so this should be fine.
        if !output.is_empty() && Some(mode_info.mode) != mode_info.base_mode {
            self.initialized = true;
        }

        pipe_message_to_plugin(MessageToPlugin::new("pipe").with_payload(format!(
            "zjstatus::pipe::pipe_{}::{}",
            self.pipe_name, output
        )));
        print!("{}", output);
    }
}

struct AnsiParser<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>,
}

impl<'a> AnsiParser<'a> {
    fn new(text: &'a str) -> Self {
        Self {
            chars: text.chars().peekable(),
        }
    }

    fn next_segment(&mut self) -> Option<AnsiSegment> {
        let ch = self.chars.next()?;

        if ch == '\x1b' {
            let mut escape_seq = String::from(ch);
            for escape_ch in self.chars.by_ref() {
                escape_seq.push(escape_ch);
                if escape_ch == 'm' {
                    break;
                }
            }
            Some(AnsiSegment::EscapeSequence(escape_seq))
        } else {
            Some(AnsiSegment::VisibleChar(ch))
        }
    }
}

enum AnsiSegment {
    EscapeSequence(String),
    VisibleChar(char),
}

fn calculate_visible_length(text: &str) -> usize {
    let mut parser = AnsiParser::new(text);
    let mut len = 0;

    while let Some(segment) = parser.next_segment() {
        if matches!(segment, AnsiSegment::VisibleChar(_)) {
            len += 1;
        }
    }

    len
}

fn truncate_ansi_string(text: &str, overflow_str: &str, max_len: usize) -> String {
    let visible_len = calculate_visible_length(text);
    let overflow_len = overflow_str.len();

    if visible_len <= max_len {
        return text.to_string();
    }

    if max_len <= overflow_len {
        return overflow_str.to_string();
    }

    let target_len = max_len - overflow_len;
    let mut result = String::new();
    let mut visible_count = 0;
    let mut parser = AnsiParser::new(text);

    while let Some(segment) = parser.next_segment() {
        match segment {
            AnsiSegment::EscapeSequence(seq) => {
                result.push_str(&seq);
            }
            AnsiSegment::VisibleChar(ch) => {
                if visible_count >= target_len {
                    break;
                }
                result.push(ch);
                visible_count += 1;
            }
        }
    }

    // Close any active ANSI styles before appending overflow marker
    result.push_str("\x1b[0m");
    result.push_str(overflow_str);
    result
}

fn find_keys_for_actions(
    keymap: &[(KeyWithModifier, Vec<Action>)],
    target_actions: &[Action],
    exact_match: bool,
) -> Vec<KeyWithModifier> {
    keymap
        .iter()
        .filter_map(|(key, key_actions)| {
            if exact_match {
                let matching = key_actions
                    .iter()
                    .zip(target_actions)
                    .filter(|(a, b)| a.shallow_eq(b))
                    .count();
                if matching == key_actions.len() && matching == target_actions.len() {
                    Some(key.clone())
                } else {
                    None
                }
            } else if key_actions.iter().next() == target_actions.iter().next() {
                Some(key.clone())
            } else {
                None
            }
        })
        .collect()
}

fn find_keys_for_action_groups(
    keymap: &[(KeyWithModifier, Vec<Action>)],
    action_groups: &[&[Action]],
) -> Vec<KeyWithModifier> {
    action_groups
        .iter()
        .flat_map(|actions| find_keys_for_actions(keymap, actions, true))
        .collect()
}

/// Format a single modifier according to the config
fn format_modifier(modifier: &KeyModifier, config: &ModifierFormatConfig) -> String {
    match modifier {
        KeyModifier::Ctrl => config.ctrl_format.clone(),
        KeyModifier::Alt => config.alt_format.clone(),
        KeyModifier::Shift => config.shift_format.clone(),
        KeyModifier::Super => config.super_format.clone(),
    }
}

fn format_modifier_string(modifiers: &[KeyModifier], config: &ModifierFormatConfig) -> String {
    if modifiers.is_empty() {
        String::new()
    } else {
        modifiers
            .iter()
            .filter_map(|m| {
                let formatted = format_modifier(m, config);
                if formatted.is_empty() {
                    None
                } else {
                    Some(formatted)
                }
            })
            .collect::<Vec<_>>()
            .join(&config.modifier_separator)
    }
}

fn format_key_display(
    key_bindings: &[KeyWithModifier],
    common_modifiers: &[KeyModifier],
) -> Vec<String> {
    key_bindings
        .iter()
        .map(|key| {
            if common_modifiers.is_empty() {
                format!("{}", key)
            } else {
                let unique_modifiers = key
                    .key_modifiers
                    .iter()
                    .filter(|m| !common_modifiers.contains(m))
                    .map(|m| m.to_string())
                    .collect::<Vec<_>>()
                    .join(" ");
                if unique_modifiers.is_empty() {
                    format!("{}", key.bare_key)
                } else {
                    format!("{} {}", unique_modifiers, key.bare_key)
                }
            }
        })
        .collect()
}

fn get_key_separator(key_display: &[String]) -> &'static str {
    let key_string = key_display.join("");
    if KEY_PATTERNS_NO_SEPARATOR.contains(&&key_string[..]) {
        ""
    } else {
        "|"
    }
}

/// Substitute placeholders in a format template
/// Supports: {mods}, {keys}, {combo}
/// {combo} uses the modifier_format_config.format_order and key_separator
fn substitute_format_template(
    template: &str,
    modifier_str: &str,
    key_display: &[String],
    key_separator: &str,
    modifier_config: &ModifierFormatConfig,
) -> String {
    let keys_str = key_display.join(key_separator);

    // Build combo string using the format_order template
    let combo_str = if !modifier_str.is_empty() && !keys_str.is_empty() {
        modifier_config.format_order
            .replace("{mods}", modifier_str)
            .replace("{sep}", &modifier_config.key_separator)
            .replace("{key}", &keys_str)
    } else if !modifier_str.is_empty() {
        modifier_str.to_string()
    } else {
        keys_str.clone()
    };

    let mut result = template.to_string();
    result = result.replace("{mods}", modifier_str);
    result = result.replace("{keys}", &keys_str);
    result = result.replace("{combo}", &combo_str);
    result
}

fn style_key_with_modifier(
    key_bindings: &[KeyWithModifier],
    palette: &Styling,
    color_config: &ColorConfig,
    label_format_config: &LabelFormatConfig,
    modifier_format_config: &ModifierFormatConfig,
    mode: Option<&str>,
    label: &str,
    strip_modifier: Option<&[KeyModifier]>,
) -> Vec<ANSIString<'static>> {
    if key_bindings.is_empty() {
        return vec![];
    }

    // Get colors for this specific label (with fallback to defaults)
    let colors = get_colors_for_label(color_config, mode, label);

    // Use custom colors if configured, otherwise fall back to palette
    let saturated_bg = colors.key_bg
        .unwrap_or_else(|| palette_match!(palette.ribbon_unselected.background));
    let contrasting_fg = colors.key_fg
        .unwrap_or_else(|| palette_match!(palette.ribbon_unselected.base));
    let mut styled_parts = vec![];

    let common_modifiers = get_common_modifiers(key_bindings.iter().collect());

    // If strip_modifier is provided, subtract those modifiers from display
    // This handles both exact matches and partial overlaps
    let display_modifiers = if let Some(strip) = strip_modifier {
        common_modifiers
            .iter()
            .filter(|m| !strip.contains(m))
            .cloned()
            .collect()
    } else {
        common_modifiers.clone()
    };

    let modifier_str = format_modifier_string(&display_modifiers, modifier_format_config);
    let key_display = format_key_display(key_bindings, &common_modifiers);
    let key_separator = get_key_separator(&key_display);

    // Get the format template for this label
    let template = get_format_for_label(label_format_config, mode, label);
    let formatted_combo = substitute_format_template(&template, &modifier_str, &key_display, key_separator, modifier_format_config);

    styled_parts.push(Style::new().paint(" "));

    // Apply bold only to the core keybinding, but render separators/glue without bold
    // This maintains visual distinction while respecting custom formatting
    styled_parts.push(
        Style::new()
            .fg(contrasting_fg)
            .on(saturated_bg)
            .bold()
            .paint(format!(" {} ", formatted_combo)),
    );

    styled_parts
}

fn style_description(
    description: &str,
    palette: &Styling,
    color_config: &ColorConfig,
    mode: Option<&str>,
    label: &str,
) -> Vec<ANSIString<'static>> {
    // Get colors for this specific label (with fallback to defaults)
    let colors = get_colors_for_label(color_config, mode, label);

    // Use custom colors if configured, otherwise fall back to palette
    let less_saturated_bg = colors.label_bg
        .unwrap_or_else(|| palette_match!(palette.text_unselected.background));
    let contrasting_fg = colors.label_fg
        .unwrap_or_else(|| palette_match!(palette.text_unselected.base));

    vec![Style::new()
        .fg(contrasting_fg)
        .on(less_saturated_bg)
        .paint(format!(" {} ", description))]
}

fn plugin_key(
    keymap: &[(KeyWithModifier, Vec<Action>)],
    plugin_name: &str,
) -> Option<KeyWithModifier> {
    keymap.iter().find_map(|(key, key_actions)| {
        if key_actions
            .iter()
            .any(|action| action.launches_plugin(plugin_name))
        {
            Some(key.clone())
        } else {
            None
        }
    })
}

fn get_select_key(keymap: &[(KeyWithModifier, Vec<Action>)]) -> Vec<KeyWithModifier> {
    let to_normal_keys = find_keys_for_actions(keymap, &[TO_NORMAL], true);
    if to_normal_keys.contains(&KeyWithModifier::new(BareKey::Enter)) {
        vec![KeyWithModifier::new(BareKey::Enter)]
    } else {
        to_normal_keys.into_iter().take(1).collect()
    }
}

fn add_hint(
    parts: &mut Vec<ANSIString<'static>>,
    keys: &[KeyWithModifier],
    description: &str,
    colors: &Styling,
    color_config: &ColorConfig,
    label_format_config: &LabelFormatConfig,
    modifier_format_config: &ModifierFormatConfig,
    label_text_overrides: &HashMap<String, String>,
    mode: Option<&str>,
    strip_modifier: Option<&[KeyModifier]>,
) {
    if !keys.is_empty() {
        // Apply label text override if available (with mode-specific lookup)
        let display_label = get_label_text(label_text_overrides, mode, description);
        let styled_keys = style_key_with_modifier(keys, colors, color_config, label_format_config, modifier_format_config, mode, description, strip_modifier);
        parts.extend(styled_keys);
        let styled_desc = style_description(&display_label, colors, color_config, mode, description);
        parts.extend(styled_desc);
    }
}

/// Convert InputMode to a mode string for color lookups
fn mode_to_str(mode: InputMode) -> Option<&'static str> {
    match mode {
        InputMode::Normal => Some("normal"),
        InputMode::Pane => Some("pane"),
        InputMode::Tab => Some("tab"),
        InputMode::Resize => Some("resize"),
        InputMode::Move => Some("move"),
        InputMode::Scroll => Some("scroll"),
        InputMode::Search => Some("search"),
        InputMode::Session => Some("session"),
        _ => None,
    }
}

/// Extract common modifiers from rendered hint keys using a threshold (80%)
/// A modifier is considered "common" if it appears in at least 80% of the hint keys.
/// This allows for some hints (like unmodified Enter for "select") to deviate without
/// eliminating the common modifier that most hints share.
fn extract_common_modifiers_from_hints(keys: &[KeyWithModifier]) -> Vec<KeyModifier> {
    if keys.is_empty() {
        return vec![];
    }

    // Threshold: modifier must appear in at least 80% of hints to be "common"
    let threshold = (keys.len() as f32 * 0.8).ceil() as usize;

    // Count occurrences of each modifier
    let mut modifier_counts: std::collections::HashMap<KeyModifier, usize> =
        std::collections::HashMap::new();

    for key in keys {
        for modifier in &key.key_modifiers {
            *modifier_counts.entry(*modifier).or_insert(0) += 1;
        }
    }

    // Keep only modifiers that meet the threshold, then sort for deterministic output
    let mut common_mods: Vec<KeyModifier> = modifier_counts
        .into_iter()
        .filter(|(_, count)| *count >= threshold)
        .map(|(modifier, _)| modifier)
        .collect();

    // Sort to ensure deterministic output (not dependent on HashMap iteration order)
    common_mods.sort();
    common_mods
}

fn render_hints_for_mode(
    mode: InputMode,
    keymap: &[(KeyWithModifier, Vec<Action>)],
    colors: &Styling,
    color_config: &ColorConfig,
    label_format_config: &LabelFormatConfig,
    modifier_format_config: &ModifierFormatConfig,
    label_text_overrides: &HashMap<String, String>,
) -> Vec<ANSIString<'static>> {
    let mut parts = vec![];
    let select_keys = get_select_key(keymap);
    let mode_str = mode_to_str(mode);

    // Collect all hints before rendering so we can extract common modifiers from only rendered keys
    let mut hints: Vec<(Vec<KeyWithModifier>, &str)> = vec![];

    match mode {
        InputMode::Normal => {
            for (action, label) in NORMAL_MODE_ACTIONS {
                let keys = find_keys_for_actions(keymap, &[action.clone()], true);
                if !keys.is_empty() {
                    hints.push((keys, label));
                }
            }
        }
        InputMode::Pane => {
            for (actions, label) in PANE_MODE_ACTION_SEQUENCES {
                let keys = find_keys_for_actions(keymap, actions, false);
                if !keys.is_empty() {
                    hints.push((keys, label));
                }
            }

            let rename_keys = find_keys_for_actions(
                keymap,
                &[
                    Action::SwitchToMode(InputMode::RenamePane),
                    Action::PaneNameInput(vec![0]),
                ],
                false,
            );
            if !rename_keys.is_empty() {
                hints.push((rename_keys, "rename"));
            }

            let focus_keys = find_keys_for_action_groups(
                keymap,
                &[
                    &[Action::MoveFocus(Direction::Left)],
                    &[Action::MoveFocus(Direction::Down)],
                    &[Action::MoveFocus(Direction::Up)],
                    &[Action::MoveFocus(Direction::Right)],
                ],
            );
            if !focus_keys.is_empty() {
                hints.push((focus_keys, "move"));
            }
            if !select_keys.is_empty() {
                hints.push((select_keys.clone(), "select"));
            }
        }
        InputMode::Tab => {
            for (actions, label) in TAB_MODE_ACTION_SEQUENCES {
                let keys = find_keys_for_actions(keymap, actions, false);
                if !keys.is_empty() {
                    hints.push((keys, label));
                }
            }

            let rename_keys = find_keys_for_actions(
                keymap,
                &[
                    Action::SwitchToMode(InputMode::RenameTab),
                    Action::TabNameInput(vec![0]),
                ],
                false,
            );
            if !rename_keys.is_empty() {
                hints.push((rename_keys, "rename"));
            }

            let focus_keys_full = find_keys_for_action_groups(
                keymap,
                &[&[Action::GoToPreviousTab], &[Action::GoToNextTab]],
            );
            let focus_keys = if focus_keys_full.contains(&KeyWithModifier::new(BareKey::Left))
                && focus_keys_full.contains(&KeyWithModifier::new(BareKey::Right))
            {
                vec![
                    KeyWithModifier::new(BareKey::Left),
                    KeyWithModifier::new(BareKey::Right),
                ]
            } else {
                focus_keys_full
            };
            if !focus_keys.is_empty() {
                hints.push((focus_keys, "move"));
            }
            if !select_keys.is_empty() {
                hints.push((select_keys.clone(), "select"));
            }
        }
        InputMode::Resize => {
            let resize_keys = find_keys_for_action_groups(
                keymap,
                &[
                    &[Action::Resize(Resize::Increase, None)],
                    &[Action::Resize(Resize::Decrease, None)],
                ],
            );
            if !resize_keys.is_empty() {
                hints.push((resize_keys, "resize"));
            }

            let increase_keys = find_keys_for_action_groups(
                keymap,
                &[
                    &[Action::Resize(Resize::Increase, Some(Direction::Left))],
                    &[Action::Resize(Resize::Increase, Some(Direction::Down))],
                    &[Action::Resize(Resize::Increase, Some(Direction::Up))],
                    &[Action::Resize(Resize::Increase, Some(Direction::Right))],
                ],
            );
            if !increase_keys.is_empty() {
                hints.push((increase_keys, "increase"));
            }

            let decrease_keys = find_keys_for_action_groups(
                keymap,
                &[
                    &[Action::Resize(Resize::Decrease, Some(Direction::Left))],
                    &[Action::Resize(Resize::Decrease, Some(Direction::Down))],
                    &[Action::Resize(Resize::Decrease, Some(Direction::Up))],
                    &[Action::Resize(Resize::Decrease, Some(Direction::Right))],
                ],
            );
            if !decrease_keys.is_empty() {
                hints.push((decrease_keys, "decrease"));
            }
            if !select_keys.is_empty() {
                hints.push((select_keys.clone(), "select"));
            }
        }
        InputMode::Move => {
            let move_keys = find_keys_for_action_groups(
                keymap,
                &[
                    &[Action::MovePane(Some(Direction::Left))],
                    &[Action::MovePane(Some(Direction::Down))],
                    &[Action::MovePane(Some(Direction::Up))],
                    &[Action::MovePane(Some(Direction::Right))],
                ],
            );
            if !move_keys.is_empty() {
                hints.push((move_keys, "move"));
            }
            if !select_keys.is_empty() {
                hints.push((select_keys.clone(), "select"));
            }
        }
        InputMode::Scroll => {
            let search_keys = find_keys_for_actions(
                keymap,
                &[
                    Action::SwitchToMode(InputMode::EnterSearch),
                    Action::SearchInput(vec![0]),
                ],
                true,
            );
            if !search_keys.is_empty() {
                hints.push((search_keys, "search"));
            }

            let scroll_keys =
                find_keys_for_action_groups(keymap, &[&[Action::ScrollDown], &[Action::ScrollUp]]);
            if !scroll_keys.is_empty() {
                hints.push((scroll_keys, "scroll"));
            }

            let page_scroll_keys = find_keys_for_action_groups(
                keymap,
                &[&[Action::PageScrollDown], &[Action::PageScrollUp]],
            );
            if !page_scroll_keys.is_empty() {
                hints.push((page_scroll_keys, "page"));
            }

            let half_page_scroll_keys = find_keys_for_action_groups(
                keymap,
                &[&[Action::HalfPageScrollDown], &[Action::HalfPageScrollUp]],
            );
            if !half_page_scroll_keys.is_empty() {
                hints.push((half_page_scroll_keys, "half page"));
            }

            let edit_keys =
                find_keys_for_actions(keymap, &[Action::EditScrollback, TO_NORMAL], false);
            if !edit_keys.is_empty() {
                hints.push((edit_keys, "edit"));
            }
            if !select_keys.is_empty() {
                hints.push((select_keys.clone(), "select"));
            }
        }
        InputMode::Search => {
            let search_keys = find_keys_for_actions(
                keymap,
                &[
                    Action::SwitchToMode(InputMode::EnterSearch),
                    Action::SearchInput(vec![0]),
                ],
                true,
            );
            if !search_keys.is_empty() {
                hints.push((search_keys, "search"));
            }

            let scroll_keys =
                find_keys_for_action_groups(keymap, &[&[Action::ScrollDown], &[Action::ScrollUp]]);
            if !scroll_keys.is_empty() {
                hints.push((scroll_keys, "scroll"));
            }

            let page_scroll_keys = find_keys_for_action_groups(
                keymap,
                &[&[Action::PageScrollDown], &[Action::PageScrollUp]],
            );
            if !page_scroll_keys.is_empty() {
                hints.push((page_scroll_keys, "page"));
            }

            let half_page_scroll_keys = find_keys_for_action_groups(
                keymap,
                &[&[Action::HalfPageScrollDown], &[Action::HalfPageScrollUp]],
            );
            if !half_page_scroll_keys.is_empty() {
                hints.push((half_page_scroll_keys, "half page"));
            }

            let down_keys =
                find_keys_for_actions(keymap, &[Action::Search(SearchDirection::Down)], true);
            if !down_keys.is_empty() {
                hints.push((down_keys, "down"));
            }

            let up_keys =
                find_keys_for_actions(keymap, &[Action::Search(SearchDirection::Up)], true);
            if !up_keys.is_empty() {
                hints.push((up_keys, "up"));
            }

            if !select_keys.is_empty() {
                hints.push((select_keys.clone(), "select"));
            }
        }
        InputMode::Session => {
            let detach_keys = find_keys_for_actions(keymap, &[Action::Detach], true);
            if !detach_keys.is_empty() {
                hints.push((detach_keys, "detach"));
            }

            if let Some(manager_key) = plugin_key(keymap, PLUGIN_SESSION_MANAGER) {
                hints.push((vec![manager_key], "manager"));
            }

            if let Some(config_key) = plugin_key(keymap, PLUGIN_CONFIGURATION) {
                hints.push((vec![config_key], "config"));
            }

            if let Some(plugin_key_val) = plugin_key(keymap, PLUGIN_MANAGER) {
                hints.push((vec![plugin_key_val], "plugins"));
            }

            if let Some(about_key) = plugin_key(keymap, PLUGIN_ABOUT) {
                hints.push((vec![about_key], "about"));
            }

            if !select_keys.is_empty() {
                hints.push((select_keys.clone(), "select"));
            }
        }
        _ => {
            let keys =
                find_keys_for_actions(keymap, &[Action::SwitchToMode(InputMode::Normal)], true);
            if !keys.is_empty() {
                hints.push((keys, "normal"));
            }
        }
    }

    // Now extract common modifiers from only the collected hints
    let all_rendered_keys: Vec<KeyWithModifier> = hints
        .iter()
        .flat_map(|(keys, _)| keys.iter().cloned())
        .collect();

    let common_modifiers = if !all_rendered_keys.is_empty() {
        extract_common_modifiers_from_hints(&all_rendered_keys)
    } else {
        vec![]
    };

    // Render the common modifier badge if present
    if !common_modifiers.is_empty() {
        let modifier_str = format_modifier_string(&common_modifiers, modifier_format_config);
        let colors_for_mods = get_colors_for_label(color_config, mode_str, "");

        // Use custom colors if configured, otherwise fall back to palette
        let key_bg = colors_for_mods.key_bg
            .unwrap_or_else(|| palette_match!(colors.ribbon_unselected.background));
        let key_fg = colors_for_mods.key_fg
            .unwrap_or_else(|| palette_match!(colors.ribbon_unselected.base));

        // Only render the badge if modifier_str is not empty (e.g., not in stripped mode)
        if !modifier_str.is_empty() {
            parts.push(Style::new()
                .fg(key_fg)
                .on(key_bg)
                .bold()
                .paint(format!(" {} ", modifier_str)));
            parts.push(Style::new().paint(" "));
        }
    }

    // Render each hint with modifiers stripped from common ones
    for (keys, label) in hints {
        add_hint(
            &mut parts,
            &keys,
            label,
            colors,
            color_config,
            label_format_config,
            modifier_format_config,
            label_text_overrides,
            mode_str,
            if common_modifiers.is_empty() {
                None
            } else {
                Some(&common_modifiers)
            },
        );
    }

    parts
}

fn get_keymap_for_mode(mode_info: &ModeInfo) -> Vec<(KeyWithModifier, Vec<Action>)> {
    match mode_info.mode {
        InputMode::Normal => mode_info.get_keybinds_for_mode(InputMode::Normal),
        InputMode::Pane => mode_info.get_keybinds_for_mode(InputMode::Pane),
        InputMode::Tab => mode_info.get_keybinds_for_mode(InputMode::Tab),
        InputMode::Resize => mode_info.get_keybinds_for_mode(InputMode::Resize),
        InputMode::Move => mode_info.get_keybinds_for_mode(InputMode::Move),
        InputMode::Scroll => mode_info.get_keybinds_for_mode(InputMode::Scroll),
        InputMode::Search => mode_info.get_keybinds_for_mode(InputMode::Search),
        InputMode::Session => mode_info.get_keybinds_for_mode(InputMode::Session),
        _ => mode_info.get_mode_keybinds(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_substitute_format_template_with_combo_only() {
        let keys = vec!["a".to_string(), "b".to_string()];
        let config = ModifierFormatConfig::default();
        let result = substitute_format_template("{combo}", "Ctrl", &keys, "|", &config);
        assert_eq!(result, "Ctrl + a|b");
    }

    #[test]
    fn test_substitute_format_template_with_no_modifiers() {
        let keys = vec!["Enter".to_string()];
        let config = ModifierFormatConfig::default();
        let result = substitute_format_template("{combo}", "", &keys, "", &config);
        assert_eq!(result, "Enter");
    }

    #[test]
    fn test_substitute_format_template_with_only_modifiers() {
        let keys: Vec<String> = vec![];
        let config = ModifierFormatConfig::default();
        let result = substitute_format_template("{combo}", "Ctrl", &keys, "", &config);
        assert_eq!(result, "Ctrl");
    }

    #[test]
    fn test_substitute_format_template_with_mods_and_keys_separate() {
        let keys = vec!["a".to_string()];
        let config = ModifierFormatConfig::default();
        let result = substitute_format_template("{mods} {keys}", "Ctrl", &keys, "", &config);
        assert_eq!(result, "Ctrl a");
    }

    #[test]
    fn test_substitute_format_template_with_custom_separator() {
        let keys = vec!["a".to_string(), "b".to_string()];
        let config = ModifierFormatConfig::default();
        let result = substitute_format_template("{mods} → {keys}", "Ctrl", &keys, "|", &config);
        assert_eq!(result, "Ctrl → a|b");
    }

    #[test]
    fn test_parse_label_format_overrides() {
        let mut config = BTreeMap::new();
        config.insert("select_key_format".to_string(), "{combo}".to_string());
        config.insert("pane.move_key_format".to_string(), "{mods} + {keys}".to_string());
        config.insert("invalid_key_format".to_string(), "no_placeholder".to_string());

        let overrides = parse_label_format_overrides(&config);

        assert_eq!(overrides.get("select").map(|f| f.template.as_str()), Some("{combo}"));
        assert_eq!(
            overrides.get("pane.move").map(|f| f.template.as_str()),
            Some("{mods} + {keys}")
        );
        // Invalid templates (missing placeholders) should not be inserted
        assert!(overrides.get("invalid").is_none());
    }

    #[test]
    fn test_get_format_for_label_mode_specific_override() {
        let mut overrides = HashMap::new();
        overrides.insert("pane.split right".to_string(), LabelFormat {
            template: "{mods} → {keys}".to_string(),
        });

        let config = LabelFormatConfig {
            defaults: LabelFormat::default(),
            overrides,
        };

        let result = get_format_for_label(&config, Some("pane"), "split right");
        assert_eq!(result, "{mods} → {keys}");
    }

    #[test]
    fn test_get_format_for_label_fallback_to_label_only() {
        let mut overrides = HashMap::new();
        overrides.insert("split right".to_string(), LabelFormat {
            template: "{combo}".to_string(),
        });

        let config = LabelFormatConfig {
            defaults: LabelFormat::default(),
            overrides,
        };

        let result = get_format_for_label(&config, Some("pane"), "split right");
        assert_eq!(result, "{combo}");
    }

    #[test]
    fn test_get_format_for_label_fallback_to_defaults() {
        let config = LabelFormatConfig {
            defaults: LabelFormat {
                template: "{combo}".to_string(),
            },
            overrides: HashMap::new(),
        };

        let result = get_format_for_label(&config, Some("pane"), "unknown");
        assert_eq!(result, "{combo}");
    }

    // Tests for modifier format configuration

    #[test]
    fn test_modifier_preset_full() {
        let config = ModifierFormatConfig::from_preset(ModifierPreset::Full);
        assert_eq!(config.ctrl_format, "Ctrl");
        assert_eq!(config.alt_format, "Alt");
        assert_eq!(config.shift_format, "Shift");
        assert_eq!(config.super_format, "Super");
        assert_eq!(config.modifier_separator, "-");
        assert_eq!(config.key_separator, " + ");
    }

    #[test]
    fn test_modifier_preset_brief() {
        let config = ModifierFormatConfig::from_preset(ModifierPreset::Brief);
        assert_eq!(config.ctrl_format, "C");
        assert_eq!(config.alt_format, "A");
        assert_eq!(config.shift_format, "S");
        assert_eq!(config.super_format, "M");
        assert_eq!(config.modifier_separator, "-");
        assert_eq!(config.key_separator, "-");
    }

    #[test]
    fn test_modifier_preset_stripped() {
        let config = ModifierFormatConfig::from_preset(ModifierPreset::Stripped);
        assert_eq!(config.ctrl_format, "");
        assert_eq!(config.alt_format, "");
        assert_eq!(config.shift_format, "");
        assert_eq!(config.super_format, "");
        assert_eq!(config.modifier_separator, "");
        assert_eq!(config.key_separator, "");
        assert_eq!(config.format_order, "{key}");
    }

    #[test]
    fn test_format_modifier_string_full() {
        let config = ModifierFormatConfig::from_preset(ModifierPreset::Full);
        let modifiers = vec![KeyModifier::Ctrl, KeyModifier::Shift];
        let result = format_modifier_string(&modifiers, &config);
        assert_eq!(result, "Ctrl-Shift");
    }

    #[test]
    fn test_format_modifier_string_brief() {
        let config = ModifierFormatConfig::from_preset(ModifierPreset::Brief);
        let modifiers = vec![KeyModifier::Ctrl, KeyModifier::Shift];
        let result = format_modifier_string(&modifiers, &config);
        assert_eq!(result, "C-S");
    }

    #[test]
    fn test_format_modifier_string_stripped() {
        let config = ModifierFormatConfig::from_preset(ModifierPreset::Stripped);
        let modifiers = vec![KeyModifier::Ctrl, KeyModifier::Shift];
        let result = format_modifier_string(&modifiers, &config);
        assert_eq!(result, "");
    }

    #[test]
    fn test_format_modifier_string_custom() {
        let config = ModifierFormatConfig {
            ctrl_format: "^".to_string(),
            alt_format: "M".to_string(),
            shift_format: "S".to_string(),
            super_format: "⌘".to_string(),
            modifier_separator: "".to_string(),
            key_separator: "".to_string(),
            format_order: "{mods}{key}".to_string(),
        };
        let modifiers = vec![KeyModifier::Ctrl];
        let result = format_modifier_string(&modifiers, &config);
        assert_eq!(result, "^");
    }

    #[test]
    fn test_parse_modifier_preset() {
        assert_eq!(parse_modifier_preset("full"), Some(ModifierPreset::Full));
        assert_eq!(parse_modifier_preset("Full"), Some(ModifierPreset::Full));
        assert_eq!(parse_modifier_preset("FULL"), Some(ModifierPreset::Full));
        assert_eq!(parse_modifier_preset("brief"), Some(ModifierPreset::Brief));
        assert_eq!(parse_modifier_preset("stripped"), Some(ModifierPreset::Stripped));
        assert_eq!(parse_modifier_preset("invalid"), None);
    }

    #[test]
    fn test_parse_modifier_format_config_with_preset() {
        let mut config = BTreeMap::new();
        config.insert("modifier_preset".to_string(), "brief".to_string());

        let result = parse_modifier_format_config(&config);
        assert_eq!(result.ctrl_format, "C");
        assert_eq!(result.key_separator, "-");
    }

    #[test]
    fn test_parse_modifier_format_config_with_custom() {
        let mut config = BTreeMap::new();
        config.insert("modifier_ctrl_format".to_string(), "^".to_string());
        config.insert("modifier_key_separator".to_string(), "".to_string());

        let result = parse_modifier_format_config(&config);
        assert_eq!(result.ctrl_format, "^");
        assert_eq!(result.key_separator, "");
        // Other values should use defaults
        assert_eq!(result.alt_format, "Alt");
    }

    #[test]
    fn test_substitute_format_template_with_custom_format_order() {
        let keys = vec!["q".to_string()];
        let config = ModifierFormatConfig {
            ctrl_format: "^".to_string(),
            alt_format: "A".to_string(),
            shift_format: "S".to_string(),
            super_format: "M".to_string(),
            modifier_separator: "".to_string(),
            key_separator: "".to_string(),
            format_order: "{mods}{sep}{key}".to_string(),
        };
        let result = substitute_format_template("{combo}", "^", &keys, "", &config);
        assert_eq!(result, "^q");
    }

    #[test]
    fn test_substitute_format_template_with_reversed_order() {
        let keys = vec!["q".to_string()];
        let config = ModifierFormatConfig {
            ctrl_format: "^".to_string(),
            alt_format: "A".to_string(),
            shift_format: "S".to_string(),
            super_format: "M".to_string(),
            modifier_separator: "".to_string(),
            key_separator: "".to_string(),
            format_order: "{key}{sep}{mods}".to_string(),
        };
        let result = substitute_format_template("{combo}", "^", &keys, "", &config);
        assert_eq!(result, "q^");
    }

    #[test]
    fn test_substitute_format_template_with_brackets() {
        // Test format like <CMS-q>
        let keys = vec!["q".to_string()];
        let config = ModifierFormatConfig {
            ctrl_format: "C".to_string(),
            alt_format: "A".to_string(),
            shift_format: "S".to_string(),
            super_format: "M".to_string(),
            modifier_separator: "".to_string(),
            key_separator: "-".to_string(),
            format_order: "<{mods}{sep}{key}>".to_string(),
        };
        let result = substitute_format_template("{combo}", "CMS", &keys, "", &config);
        assert_eq!(result, "<CMS-q>");
    }

    #[test]
    fn test_format_order_with_prefix_suffix_no_mods() {
        // When there are no modifiers, combo should just be the key (not wrapped)
        let keys = vec!["Enter".to_string()];
        let config = ModifierFormatConfig {
            ctrl_format: "C".to_string(),
            alt_format: "A".to_string(),
            shift_format: "S".to_string(),
            super_format: "M".to_string(),
            modifier_separator: "".to_string(),
            key_separator: "-".to_string(),
            format_order: "<{mods}{sep}{key}>".to_string(),
        };
        // When modifier_str is empty, the combo just returns the key
        let result = substitute_format_template("{combo}", "", &keys, "", &config);
        assert_eq!(result, "Enter");
    }
}
