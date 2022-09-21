(module rko.plugin.autopairs
  {autoload {nvim aniseed.nvim
             autopairs nvim-autopairs}})

(autopairs.setup
  {:disable_filetype ["TelescopePrompt"]
   :map_cr true
   :map_bs true
   :enable_check_bracket_line false})
