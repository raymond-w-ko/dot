local wezterm = require 'wezterm';
return {
  freetype_load_target = "Light",
  freetype_render_target = "HorizontalLcd",
  font = wezterm.font("Iosevka Term", {}),
  font_size = 9.0,
  line_height = 0.9,
  enable_scrollbar = false,
    window_padding = {
    left = 0,
    right = 0,
    top = 0,
    bottom = 0,
  },
  hide_tab_bar_if_only_one_tab = true,
  -- color_scheme = "Gruvbox Light",
  colors = {
    foreground = "#504945",
    background = "#f2e5bc",

    cursor_bg = "#282828",
    cursor_fg = "#fbf1c7",
    cursor_border = "#282828",

    selection_fg = "#d5c4a1",
    selection_bg = "#665c54",

    scrollbar_thumb = "#222222",

    split = "#444444",

    ansi = {"#fbf1c7", "#cc241d", "#98971a", "#d79921", "#458588", "#b16286", "#689d6a", "#7c6f64"},
    brights = {"#928374", "#9d0006", "#79740e", "#b57614", "#076678", "#8f3f71", "#427b58", "#3c3836"},

    indexed = {[136] = "#af8700"},

    compose_cursor = "orange",
  },
  default_prog = {"wsl.exe"},
}