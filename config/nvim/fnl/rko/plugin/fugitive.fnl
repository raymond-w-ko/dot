(module rko.plugin.fugitive
  {autoload {nvim aniseed.nvim}})

(nvim.set_keymap :n :<leader>gs ":Git<cr>" {})
(nvim.set_keymap :n :<leader>gp ":G push<cr>" {})
