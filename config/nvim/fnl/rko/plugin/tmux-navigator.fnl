(module rko.plugin.tmux-navigator
  {autoload {nvim aniseed.nvim}})

(set nvim.g.tmux_navigator_no_mappings 1)
(vim.keymap.set :n "<left>" ":TmuxNavigateLeft<cr>" {:silent true})
(vim.keymap.set :n "<right>" ":TmuxNavigateRight<cr>" {:silent true})

;; (vim.keymap.set :n "<up>" ":TmuxNavigateUp<cr>" {:silent true})
(vim.keymap.set :n "<up>" "<c-w><c-p>" {:silent true})
(vim.keymap.set :n "<down>" ":TmuxNavigateDown<cr>" {:silent true})
