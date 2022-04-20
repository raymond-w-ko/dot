(module rko.plugin.qf
  {autoload {nvim aniseed.nvim}})

(vim.keymap.set :n "<home>" "<plug>(qf_qf_previous)")
(vim.keymap.set :n "<end>" "<plug>(qf_qf_next)")
