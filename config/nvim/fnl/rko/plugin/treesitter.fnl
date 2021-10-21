(module rko.plugin.treesitter
  {autoload {treesitter nvim-treesitter.configs}})

(treesitter.setup
  {:ensure_installed "maintained"
   :highlight {:enable true
               :disable ["scss"]} 
   :indent  {:enable true}})
