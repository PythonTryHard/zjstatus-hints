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

/// Get the display label text, with override support
fn get_label_text(overrides: &HashMap<String, String>, label: &str) -> String {
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
        self.label_format_config = LabelFormatConfig {
            defaults: LabelFormat {
                template: configuration
                    .get("key_format")
                    .cloned()
                    .unwrap_or_else(|| "{combo}".to_string()),
            },
            overrides: parse_label_format_overrides(&configuration),
        };

        // Parse custom label text overrides
        self.label_text_overrides = parse_label_text_overrides(&configuration);

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
            let parts = render_hints_for_mode(mode_info.mode, &keymap, &mode_info.style.colors, &self.color_config, &self.label_format_config, &self.label_text_overrides);

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

fn format_modifier_string(modifiers: &[KeyModifier]) -> String {
    if modifiers.is_empty() {
        String::new()
    } else {
        modifiers
            .iter()
            .map(|m| m.to_string())
            .collect::<Vec<_>>()
            .join("-")
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
/// {combo} is {mods} + {keys} when both are present, otherwise just {keys}
fn substitute_format_template(
    template: &str,
    modifier_str: &str,
    key_display: &[String],
    key_separator: &str,
) -> String {
    let keys_str = key_display.join(key_separator);

    let combo_str = if !modifier_str.is_empty() && !keys_str.is_empty() {
        format!("{} + {}", modifier_str, keys_str)
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
    mode: Option<&str>,
    label: &str,
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
    let modifier_str = format_modifier_string(&common_modifiers);
    let key_display = format_key_display(key_bindings, &common_modifiers);
    let key_separator = get_key_separator(&key_display);

    // Get the format template for this label
    let template = get_format_for_label(label_format_config, mode, label);
    let formatted_combo = substitute_format_template(&template, &modifier_str, &key_display, key_separator);

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

    styled_parts.push(Style::new().fg(contrasting_fg).on(saturated_bg).paint(" "));

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
    label_text_overrides: &HashMap<String, String>,
    mode: Option<&str>,
) {
    if !keys.is_empty() {
        // Apply label text override if available
        let display_label = get_label_text(label_text_overrides, description);
        let styled_keys = style_key_with_modifier(keys, colors, color_config, label_format_config, mode, description);
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

fn render_hints_for_mode(
    mode: InputMode,
    keymap: &[(KeyWithModifier, Vec<Action>)],
    colors: &Styling,
    color_config: &ColorConfig,
    label_format_config: &LabelFormatConfig,
    label_text_overrides: &HashMap<String, String>,
) -> Vec<ANSIString<'static>> {
    let mut parts = vec![];
    let select_keys = get_select_key(keymap);
    let mode_str = mode_to_str(mode);

    match mode {
        InputMode::Normal => {
            for (action, label) in NORMAL_MODE_ACTIONS {
                let keys = find_keys_for_actions(keymap, &[action.clone()], true);
                add_hint(&mut parts, &keys, label, colors, color_config, label_format_config, label_text_overrides, mode_str);
            }
        }
        InputMode::Pane => {
            for (actions, label) in PANE_MODE_ACTION_SEQUENCES {
                let keys = find_keys_for_actions(keymap, actions, false);
                if !keys.is_empty() {
                    add_hint(&mut parts, &keys, label, colors, color_config, label_format_config, label_text_overrides, mode_str);
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
                add_hint(&mut parts, &rename_keys, "rename", colors, color_config, label_format_config, label_text_overrides, mode_str);
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
            add_hint(&mut parts, &focus_keys, "move", colors, color_config, label_format_config, label_text_overrides, mode_str);
            add_hint(&mut parts, &select_keys, "select", colors, color_config, label_format_config, label_text_overrides, mode_str);
        }
        InputMode::Tab => {
            for (actions, label) in TAB_MODE_ACTION_SEQUENCES {
                let keys = find_keys_for_actions(keymap, actions, false);
                if !keys.is_empty() {
                    add_hint(&mut parts, &keys, label, colors, color_config, label_format_config, label_text_overrides, mode_str);
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
                add_hint(&mut parts, &rename_keys, "rename", colors, color_config, label_format_config, label_text_overrides, mode_str);
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
            add_hint(&mut parts, &focus_keys, "move", colors, color_config, label_format_config, label_text_overrides, mode_str);
            add_hint(&mut parts, &select_keys, "select", colors, color_config, label_format_config, label_text_overrides, mode_str);
        }
        InputMode::Resize => {
            let resize_keys = find_keys_for_action_groups(
                keymap,
                &[
                    &[Action::Resize(Resize::Increase, None)],
                    &[Action::Resize(Resize::Decrease, None)],
                ],
            );
            add_hint(&mut parts, &resize_keys, "resize", colors, color_config, label_format_config, label_text_overrides, mode_str);

            let increase_keys = find_keys_for_action_groups(
                keymap,
                &[
                    &[Action::Resize(Resize::Increase, Some(Direction::Left))],
                    &[Action::Resize(Resize::Increase, Some(Direction::Down))],
                    &[Action::Resize(Resize::Increase, Some(Direction::Up))],
                    &[Action::Resize(Resize::Increase, Some(Direction::Right))],
                ],
            );
            add_hint(&mut parts, &increase_keys, "increase", colors, color_config, label_format_config, label_text_overrides, mode_str);

            let decrease_keys = find_keys_for_action_groups(
                keymap,
                &[
                    &[Action::Resize(Resize::Decrease, Some(Direction::Left))],
                    &[Action::Resize(Resize::Decrease, Some(Direction::Down))],
                    &[Action::Resize(Resize::Decrease, Some(Direction::Up))],
                    &[Action::Resize(Resize::Decrease, Some(Direction::Right))],
                ],
            );
            add_hint(&mut parts, &decrease_keys, "decrease", colors, color_config, label_format_config, label_text_overrides, mode_str);
            add_hint(&mut parts, &select_keys, "select", colors, color_config, label_format_config, label_text_overrides, mode_str);
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
            add_hint(&mut parts, &move_keys, "move", colors, color_config, label_format_config, label_text_overrides, mode_str);
            add_hint(&mut parts, &select_keys, "select", colors, color_config, label_format_config, label_text_overrides, mode_str);
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
            add_hint(&mut parts, &search_keys, "search", colors, color_config, label_format_config, label_text_overrides, mode_str);

            let scroll_keys =
                find_keys_for_action_groups(keymap, &[&[Action::ScrollDown], &[Action::ScrollUp]]);
            add_hint(&mut parts, &scroll_keys, "scroll", colors, color_config, label_format_config, label_text_overrides, mode_str);

            let page_scroll_keys = find_keys_for_action_groups(
                keymap,
                &[&[Action::PageScrollDown], &[Action::PageScrollUp]],
            );
            add_hint(&mut parts, &page_scroll_keys, "page", colors, color_config, label_format_config, label_text_overrides, mode_str);

            let half_page_scroll_keys = find_keys_for_action_groups(
                keymap,
                &[&[Action::HalfPageScrollDown], &[Action::HalfPageScrollUp]],
            );
            add_hint(&mut parts, &half_page_scroll_keys, "half page", colors, color_config, label_format_config, label_text_overrides, mode_str);

            let edit_keys =
                find_keys_for_actions(keymap, &[Action::EditScrollback, TO_NORMAL], false);
            if !edit_keys.is_empty() {
                add_hint(&mut parts, &edit_keys, "edit", colors, color_config, label_format_config, label_text_overrides, mode_str);
            }
            add_hint(&mut parts, &select_keys, "select", colors, color_config, label_format_config, label_text_overrides, mode_str);
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
            add_hint(&mut parts, &search_keys, "search", colors, color_config, label_format_config, label_text_overrides, mode_str);

            let scroll_keys =
                find_keys_for_action_groups(keymap, &[&[Action::ScrollDown], &[Action::ScrollUp]]);
            add_hint(&mut parts, &scroll_keys, "scroll", colors, color_config, label_format_config, label_text_overrides, mode_str);

            let page_scroll_keys = find_keys_for_action_groups(
                keymap,
                &[&[Action::PageScrollDown], &[Action::PageScrollUp]],
            );
            add_hint(&mut parts, &page_scroll_keys, "page", colors, color_config, label_format_config, label_text_overrides, mode_str);

            let half_page_scroll_keys = find_keys_for_action_groups(
                keymap,
                &[&[Action::HalfPageScrollDown], &[Action::HalfPageScrollUp]],
            );
            add_hint(&mut parts, &half_page_scroll_keys, "half page", colors, color_config, label_format_config, label_text_overrides, mode_str);

            let down_keys =
                find_keys_for_actions(keymap, &[Action::Search(SearchDirection::Down)], true);
            add_hint(&mut parts, &down_keys, "down", colors, color_config, label_format_config, label_text_overrides, mode_str);

            let up_keys =
                find_keys_for_actions(keymap, &[Action::Search(SearchDirection::Up)], true);
            add_hint(&mut parts, &up_keys, "up", colors, color_config, label_format_config, label_text_overrides, mode_str);

            add_hint(&mut parts, &select_keys, "select", colors, color_config, label_format_config, label_text_overrides, mode_str);
        }
        InputMode::Session => {
            let detach_keys = find_keys_for_actions(keymap, &[Action::Detach], true);
            add_hint(&mut parts, &detach_keys, "detach", colors, color_config, label_format_config, label_text_overrides, mode_str);

            if let Some(manager_key) = plugin_key(keymap, PLUGIN_SESSION_MANAGER) {
                add_hint(&mut parts, &[manager_key], "manager", colors, color_config, label_format_config, label_text_overrides, mode_str);
            }

            if let Some(config_key) = plugin_key(keymap, PLUGIN_CONFIGURATION) {
                add_hint(&mut parts, &[config_key], "config", colors, color_config, label_format_config, label_text_overrides, mode_str);
            }

            if let Some(plugin_key_val) = plugin_key(keymap, PLUGIN_MANAGER) {
                add_hint(&mut parts, &[plugin_key_val], "plugins", colors, color_config, label_format_config, label_text_overrides, mode_str);
            }

            if let Some(about_key) = plugin_key(keymap, PLUGIN_ABOUT) {
                add_hint(&mut parts, &[about_key], "about", colors, color_config, label_format_config, label_text_overrides, mode_str);
            }

            add_hint(&mut parts, &select_keys, "select", colors, color_config, label_format_config, label_text_overrides, mode_str);
        }
        _ => {
            let keys =
                find_keys_for_actions(keymap, &[Action::SwitchToMode(InputMode::Normal)], true);
            add_hint(&mut parts, &keys, "normal", colors, color_config, label_format_config, label_text_overrides, mode_str);
        }
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
        let result = substitute_format_template("{combo}", "Ctrl", &keys, "|");
        assert_eq!(result, "Ctrl + a|b");
    }

    #[test]
    fn test_substitute_format_template_with_no_modifiers() {
        let keys = vec!["Enter".to_string()];
        let result = substitute_format_template("{combo}", "", &keys, "");
        assert_eq!(result, "Enter");
    }

    #[test]
    fn test_substitute_format_template_with_only_modifiers() {
        let keys: Vec<String> = vec![];
        let result = substitute_format_template("{combo}", "Ctrl", &keys, "");
        assert_eq!(result, "Ctrl");
    }

    #[test]
    fn test_substitute_format_template_with_mods_and_keys_separate() {
        let keys = vec!["a".to_string()];
        let result = substitute_format_template("{mods} {keys}", "Ctrl", &keys, "");
        assert_eq!(result, "Ctrl a");
    }

    #[test]
    fn test_substitute_format_template_with_custom_separator() {
        let keys = vec!["a".to_string(), "b".to_string()];
        let result = substitute_format_template("{mods} → {keys}", "Ctrl", &keys, "|");
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
}
