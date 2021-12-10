(module rko.plugin.lazygit
  {autoload {nvim aniseed.nvim}})

(nvim.set_keymap :n "<leader>g" ":LazyGit<cr>" {:noremap true})
