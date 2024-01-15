local wezterm = require 'wezterm';
local config = {
  freetype_load_target = "Normal",
  freetype_render_target = "Normal",
  font = wezterm.font("FiraCode Nerd Font", {weight="Medium"}),
  --font = wezterm.font("Iosevka Term", {weight="Thin", stretch="Normal", style="Normal"}),
  font_size = 10.0,
  line_height = 1.0,
  initial_rows=39,
  initial_cols=100,
  canonicalize_pasted_newlines = "CarriageReturn",
  window_close_confirmation = "NeverPrompt",
  enable_scroll_bar = false,
  window_padding = {
    left = 0,
    right = 0,
    top = 0,
    bottom = 0,
  },
  default_cursor_style = "SteadyBar",
  hide_tab_bar_if_only_one_tab = true,
  default_prog = {"wsl.exe"},
}

config.colors = {
  foreground = '#adbcbc', -- fg_0
  background = '#103c48', -- bg_0

  -- Overrides the cell background color when the current cell is occupied by the
  -- cursor and the cursor style is set to Block
  cursor_bg = '#ffff00',
  -- Overrides the text color when the current cell is occupied by the cursor
  cursor_fg = '#000000',
  -- Specifies the border color of the cursor when the cursor style is set to Block,
  -- or the color of the vertical or horizontal bar when the cursor style is set to
  -- Bar or Underline.
  cursor_border = '#ffff00',

  -- the foreground color of selected text
  selection_fg = 'black',
  -- the background color of selected text
  selection_bg = '#fffacd',

  -- The color of the scrollbar "thumb"; the portion that represents the current viewport
  scrollbar_thumb = '#222222',

  -- The color of the split lines between panes
  split = '#000000',

  ansi = {
    '#184956', -- bg_1
    '#fa5750', -- red
    '#75b938', -- green
    '#dbb32d', -- yellow
    '#4695f7', -- blue
    '#f275be', -- magenta
    '#41c7b9', -- cyan
    '#72898f', -- dim_0
  },
  brights = {
    '#2d5b69', -- bg_2
    '#ff665c', -- br_red
    '#84c747', -- br_green
    '#ebc13d', -- br_yellow
    '#58a3ff', -- br_blue
    '#ff84cd', -- br_magenta
    '#53d6c7', -- br_cyan
    '#cad8d9', -- fg_1
  },
  -- orange
  -- #da6930
  -- br_orange
  -- #f37b3f

  -- Arbitrary colors of the palette in the range from 16 to 255
  -- indexed = { [136] = '#af8700' },

  -- Since: 20220319-142410-0fcdea07
  -- When the IME, a dead key or a leader key are being processed and are effectively
  -- holding input pending the result of input composition, change the cursor
  -- to this color to give a visual cue about the compose state.
  compose_cursor = '#da6930',

  -- Colors for copy_mode and quick_select
  -- available since: 20220807-113146-c2fee766
  -- In copy_mode, the color of the active text is:
  -- 1. copy_mode_active_highlight_* if additional text was selected using the mouse
  -- 2. selection_* otherwise
  copy_mode_active_highlight_bg = { Color = '#000000' },
  -- use `AnsiColor` to specify one of the ansi color palette values
  -- (index 0-15) using one of the names "Black", "Maroon", "Green",
  --  "Olive", "Navy", "Purple", "Teal", "Silver", "Grey", "Red", "Lime",
  -- "Yellow", "Blue", "Fuchsia", "Aqua" or "White".
  copy_mode_active_highlight_fg = { AnsiColor = 'Black' },
  copy_mode_inactive_highlight_bg = { Color = '#52ad70' },
  copy_mode_inactive_highlight_fg = { AnsiColor = 'White' },

  quick_select_label_bg = { Color = 'peru' },
  quick_select_label_fg = { Color = '#ffffff' },
  quick_select_match_bg = { AnsiColor = 'Navy' },
  quick_select_match_fg = { Color = '#ffffff' },
}

return config