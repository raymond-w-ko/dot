(module rko.plugin.lazygit
  {autoload {nvim aniseed.nvim}})

(nvim.set_keymap :n "<leader>gg" ":LazyGit<cr>" {:noremap true})
