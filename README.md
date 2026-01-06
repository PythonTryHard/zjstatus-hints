# zjstatus-hints

A [Zellij](https://github.com/zellij-org/zellij) plugin that displays context-aware key bindings for each Zellij mode. Extends the functionality of [zjstatus](https://github.com/dj95/zjstatus).

![2025-06-06_16-23-55_region](https://github.com/user-attachments/assets/cfb93423-f37c-410a-aca9-a49290312d0e)

https://github.com/user-attachments/assets/940a31a0-86de-469d-89e2-dab18a1aaca8

## Rationale

Zjstatus is an excellent plugin, but it lacks the ability to display keybinding hints for your current mode, as the built-in Zellij status-bar plugin allows. This plugin adds that functionality to zjstatus, so you can have the best of both worlds.

## Features

- Shows context-aware key bindings for each Zellij mode (Normal, Pane, Tab, Resize, Move, Scroll, Search, Session)
- Integrates seamlessly with zjstatus via named pipes

## Installation

First, install and configure [zjstatus](https://github.com/dj95/zjstatus). Then, add the zjstatus-hints plugin to your Zellij configuration:

```kdl
plugins {
    zjstatus-hints location="https://github.com/b0o/zjstatus-hints/releases/latest/download/zjstatus-hints.wasm" {
        // Maximum number of characters to display
        max_length 0 // 0 = unlimited
        // String to append when truncated
        overflow_str "..." // default
        // Name of the pipe for zjstatus integration
        pipe_name "zjstatus_hints" // default
        // Hide hints in base mode (a.k.a. default mode)
        // E.g. if you have set default_mode to "locked", then
        // you can hide hints in the locked mode by setting this to true
        hide_in_base_mode false // default

        // Custom colors (hex format: "#RRGGBB" or "RRGGBB")
        // These override the default Zellij theme colors
        // key_fg "#000000"   // Foreground color for keybinding badges
        // key_bg "#00D9FF"   // Background color for keybinding badges
        // label_fg "#E0E0FF" // Foreground color for hint labels
        // label_bg "#0A0A0F" // Background color for hint labels

        // Per-label color overrides (applies to specific hints)
        // select_key_bg "#FF0000"  // Red background for "select" hint in all modes

        // Mode-specific color overrides (applies only in specific modes)
        // pane.new_key_bg "#00FF00"  // Green background for "new" hint only in pane mode
    }
}

load_plugins {
    // Load at startup
    zjstatus-hints
}
```

Finally, configure zjstatus to display the hints in your default layout (`layouts/default.kdl`):

```kdl
layout {
    default_tab_template {
        children
        pane size=1 borderless=true {
            plugin location="zjstatus" {
                format_left   "{mode} {tabs}"

                // You can put `{pipe_zjstatus_hints}` inside of format_left, format_center, or format_right.
                // The pipe name should match the pipe_name configuration option from above, which is zjstatus_hints by default.
                // e.g. pipe_<pipe_name>
                format_right  "{pipe_zjstatus_hints}{datetime} " 

                // Note: this is necessary or else zjstatus won't render the pipe:
                pipe_zjstatus_hints_format "{output}"
                // Use "raw" rendermode when using custom colors to preserve ANSI codes:
                // pipe_zjstatus_hints_rendermode "raw"
            }
        }
    }
}
```

## Configuration

- `max_length`: Maximum number of characters to display (default: 0 = unlimited)
- `overflow_str`: String to append when truncated (default: "...")
- `pipe_name`: Name of the pipe for zjstatus integration (default: "zjstatus_hints")
- `hide_in_base_mode`: Hide hints in base mode (a.k.a. default mode) (default: false)

### Color Configuration

Global color options (apply to all hints):
- `key_fg`: Foreground color for keybinding badges (hex format, e.g. "#000000")
- `key_bg`: Background color for keybinding badges (hex format, e.g. "#00D9FF")
- `label_fg`: Foreground color for hint labels (hex format, e.g. "#E0E0FF")
- `label_bg`: Background color for hint labels (hex format, e.g. "#0A0A0F")

Per-label color overrides (apply to specific hints in all modes):
- `{label}_key_fg`, `{label}_key_bg`, `{label}_label_fg`, `{label}_label_bg`
- For labels with spaces, use underscores: `split_right_key_bg` for "split right"
- Example: `select_key_bg "#FF0000"` applies to the "select" hint everywhere

Mode-global color defaults (apply to all hints in a specific mode):
- `{mode}.key_fg`, `{mode}.key_bg`, `{mode}.label_fg`, `{mode}.label_bg`
- Example: `pane.key_bg "#00FF00"` applies to all hints in pane mode
- Valid modes: `normal`, `pane`, `tab`, `resize`, `move`, `scroll`, `search`, `session`

Mode+label specific color overrides (apply to a specific hint in a specific mode):
- `{mode}.{label}_key_fg`, `{mode}.{label}_key_bg`, etc.
- Example: `pane.new_key_bg "#FF0000"` applies only to "new" hint in pane mode
- For labels with spaces, use underscores: `pane.split_right_key_bg` for "split right" in pane mode

Color priority (highest to lowest):
1. Mode+label specific override (e.g., `pane.select_key_bg`)
2. Mode-global default (e.g., `pane.key_bg`)
3. Per-label override (e.g., `select_key_bg`)
4. Global color (e.g., `key_bg`)
5. Zellij theme colors (`ribbon_unselected` for keys, `text_unselected` for labels)

**Note:** When using custom colors, you should set `pipe_zjstatus_hints_rendermode "raw"` in your zjstatus configuration to ensure the ANSI color codes are rendered correctly.

### Label Text Configuration

Customize the text displayed for hint labels (e.g., "pane" → "p" or use unicode glyphs).

Global label text option (applies to all hints):
- Default labels are used from Zellij's action names (e.g., "pane", "tab", "new", "split right")

Per-label text overrides (apply to specific hints in all modes):
- `{label}_label_text`
- For labels with spaces, use underscores: `split_right_label_text` for "split right"
- Example: `pane_label_text "ｐ"` replaces "pane" with "ｐ" (unicode fullwidth character)
- Supports unicode glyphs: `new_label_text "✨"`, `close_label_text "✕"`, etc.

Mode-global label text defaults (apply to all hints in a specific mode):
- `{mode}.label_text`
- Example: `pane.label_text "→"` sets default text for all hints in pane mode
- Valid modes: `normal`, `pane`, `tab`, `resize`, `move`, `scroll`, `search`, `session`

Mode+label specific label text overrides (apply to a specific hint in a specific mode):
- `{mode}.{label}_label_text`
- Example: `pane.new_label_text "ｎ"` applies only to "new" hint in pane mode
- For labels with spaces, use underscores: `pane.split_right_label_text`

Label text priority (highest to lowest):
1. Mode+label specific override (e.g., `pane.new_label_text`)
2. Mode-global default (e.g., `pane.label_text`)
3. Per-label override (e.g., `new_label_text`)
4. Default label from Zellij (e.g., "new")

#### Label Text Examples

```kdl
plugins {
    zjstatus-hints location="..." {
        // Shorthand ASCII labels
        pane_label_text "p"
        tab_label_text "t"
        resize_label_text "r"
        move_label_text "m"
        scroll_label_text "sc"
        search_label_text "sh"

        // Unicode fullwidth characters
        pane_label_text "ｐ"
        tab_label_text "ｔ"
        new_label_text "ｎ"

        // Unicode symbols
        split_right_label_text "→"
        split_down_label_text "↓"
        close_label_text "✕"
        fullscreen_label_text "◻"

        // Mode-specific overrides: different labels per mode
        pane.move_label_text "↕"
        pane.new_label_text "+"
        tab.move_label_text "←→"
    }
}
```

### Label Format Configuration

Customize how keybind displays are formatted using template syntax.

Global format option (applies to all hints):
- `key_format`: Template for formatting keybindings (default: `"{combo}"`)

Per-label format overrides (apply to specific hints in all modes):
- `{label}_key_format`
- For labels with spaces, use underscores: `split_right_key_format` for "split right"
- Example: `select_key_format "{mods} → {keys}"` customizes the "select" hint

Mode-global format defaults (apply to all hints in a specific mode):
- `{mode}.key_format`
- Example: `pane.key_format "{mods} + {keys}"` applies to all hints in pane mode
- Valid modes: `normal`, `pane`, `tab`, `resize`, `move`, `scroll`, `search`, `session`

Mode+label specific format overrides (apply to a specific hint in a specific mode):
- `{mode}.{label}_key_format`
- Example: `pane.new_key_format "{combo}"` applies only to "new" hint in pane mode
- For labels with spaces, use underscores: `pane.split_right_key_format`

#### Template Placeholders

- `{combo}`: Combined modifier and key display (modifiers + keys when both present, otherwise just keys)
- `{mods}`: Modifier keys only (e.g., "Ctrl", "Ctrl-Shift")
- `{keys}`: Keybindings only with default separator (e.g., "a|b" or "Enter")

#### Examples

```kdl
plugins {
    zjstatus-hints location="..." {
        // Default format: shows modifiers + keys when both present, otherwise just keys
        key_format "{combo}"

        // Alternative: always show modifiers on the left, separated by arrow
        // key_format "{mods} → {keys}"

        // Per-label override: different format for "split right" hint
        split_right_key_format "{mods}{keys}"

        // Mode-global default: use this format for all hints in pane mode
        pane.key_format "{mods} + {keys}"

        // Mode+label specific override: use compact format only for "move" in pane mode
        pane.move_key_format "{combo}"

        // Custom separator in template
        // This would render as "Ctrl ➜ hjkl" for a Ctrl+hjkl binding
        // pane.move_key_format "{mods} ➜ {keys}"
    }
}
```

Format priority (highest to lowest):
1. Mode+label specific override (e.g., `pane.select_key_format`)
2. Mode-global default (e.g., `pane.key_format`)
3. Per-label override (e.g., `select_key_format`)
4. Global format (e.g., `key_format`)
4. Built-in default (`"{combo}"`)

**Note:** Template placeholders must include at least one of `{keys}` or `{combo}` to be valid. Invalid templates are ignored and the default is used instead.

### Modifier Format Configuration

Customize how modifier keys (Ctrl, Alt, Shift, Super) are displayed. Each option falls back to its default if not specified. To explicitly use an empty string, set the value to `""`.

#### Format Options

| Option | Description | Default |
|--------|-------------|---------|
| `modifier_ctrl_format` | Format for Ctrl modifier | "Ctrl" |
| `modifier_alt_format` | Format for Alt modifier | "Alt" |
| `modifier_shift_format` | Format for Shift modifier | "Shift" |
| `modifier_super_format` | Format for Super modifier | "Super" |
| `modifier_separator` | Separator between modifiers | "-" |
| `modifier_key_separator` | Separator between modifiers and key | " + " |
| `modifier_combo_template` | Template for composing the combo string | "{mods}{sep}{key}" |
| `key_display_separator` | Separator between multiple keys for the same action | "\|" |

##### Combo Template

The `modifier_combo_template` option is a template string for composing modifier+key combinations:
- `{mods}`: The formatted modifier string (e.g., "Ctrl", "C-S")
- `{sep}`: The `modifier_key_separator` value
- `{key}`: The key(s) being bound

You can include arbitrary prefix/suffix characters in the template (e.g., `<{mods}{sep}{key}>` for Vim-style).

**Literal curly braces:** Only the exact placeholders `{mods}`, `{sep}`, and `{key}` are replaced. Other curly braces are preserved as-is. For example, `{{mods}}` produces `{C}` (the outer braces are literal, only the inner `{mods}` is replaced).

##### Key Display Separator

The `key_display_separator` option controls the separator between multiple keys bound to the same action. For example, if both `h` and `←` move left, they would be displayed as `h|←` by default, or `h / ←` with `key_display_separator " / "`.

#### Examples

```kdl
plugins {
    zjstatus-hints location="..." {
        // Brief style: C-q, C-A-x
        modifier_ctrl_format "C"
        modifier_alt_format "A"
        modifier_shift_format "S"
        modifier_super_format "M"
        modifier_key_separator "-"
    }
}
```

```kdl
plugins {
    zjstatus-hints location="..." {
        // Caret-style: ^q, ^x
        modifier_ctrl_format "^"
        modifier_alt_format "M"
        modifier_shift_format "S"
        modifier_super_format "⌘"
        modifier_separator ""
        modifier_key_separator ""
        modifier_combo_template "{mods}{key}"
    }
}
```

```kdl
plugins {
    zjstatus-hints location="..." {
        // Key-first format: q^, x^
        modifier_ctrl_format "^"
        modifier_key_separator ""
        modifier_combo_template "{key}{mods}"
    }
}
```

```kdl
plugins {
    zjstatus-hints location="..." {
        // Bracketed format: [C]q
        modifier_ctrl_format "[C]"
        modifier_key_separator ""
        modifier_combo_template "{mods}{key}"
    }
}
```

```kdl
plugins {
    zjstatus-hints location="..." {
        // Vim-style angle bracket format: <C-q>, <C-S-x>
        modifier_ctrl_format "C"
        modifier_alt_format "A"
        modifier_shift_format "S"
        modifier_super_format "M"
        modifier_separator "-"
        modifier_key_separator "-"
        modifier_combo_template "<{mods}{sep}{key}>"
    }
}
```

## TODO

- [x] configurable colors/formatting
- [x] more advanced mode-specific configuration
- [ ] improved handling of long outputs
- [ ] ability to enable/disable specific hints

## License

&copy; 2025 Maddison Hellstrom

Adapted from the built-in Zellij status-bar plugin by Brooks J Rady.

MIT License
