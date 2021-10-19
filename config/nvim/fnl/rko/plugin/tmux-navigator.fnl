(module rko.plugin.tmux-navigator
  {autoload {nvim aniseed.nvim}})

(set nvim.g.tmux_navigator_no_mappings 1)
(nvim.set_keymap :n :<Left> ":TmuxNavigateLeft<cr>" {:noremap true :silent true})
(nvim.set_keymap :n :<Right> ":TmuxNavigateRight<cr>" {:noremap true :silent true})
(nvim.set_keymap :n :<Up> ":TmuxNavigateUp<cr>" {:noremap true :silent true})
(nvim.set_keymap :n :<Down> ":TmuxNavigateDown<cr>" {:noremap true :silent true})
