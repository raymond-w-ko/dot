(module rko.plugin.treesitter
  {autoload {treesitter nvim-treesitter.configs}})

(treesitter.setup
  {:ensure_installed ["clojure" "python" "javascript" "css" "scss" "json5" "vim"]
   :highlight {:enable true
               :disable ["scss" "javascript"]} 
   :indent  {:enable true}})
