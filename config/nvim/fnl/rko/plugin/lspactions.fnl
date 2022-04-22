(module rko.plugin.lspactions
  {autoload {treesitter nvim-treesitter.configs
             lspactions lspactions}})

(vim.keymap.set :n "<leader>ar" lspactions.rename)
