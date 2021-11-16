(module rko.plugin.autopairs
  {autoload {nvim aniseed.nvim
             autopairs nvim-autopairs}})

(autopairs.setup
  {:disable_filetype ["TelescopePrompt" "clojure" "fennel"]
   :enable_check_bracket_line false})
