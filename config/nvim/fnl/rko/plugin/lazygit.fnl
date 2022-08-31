(module rko.plugin.lazygit
  {autoload {nvim aniseed.nvim}})

(nvim.set_keymap :n "<leader>g" ":LazyGit<cr>" {:noremap true})
(set nvim.g.lazygit_floating_window_use_plenary 0)
(set nvim.g.lazygit_floating_window_winblend 1)
