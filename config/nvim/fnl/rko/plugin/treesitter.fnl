(module rko.plugin.treesitter
  {autoload {treesitter nvim-treesitter.configs}})

(treesitter.setup
  {:ensure_installed ["clojure" "python" "json5" "vim" "yaml"
                      "bash" "c" "c_sharp" "cpp" "fennel" "go" "rust" "lua" "markdown"
                      "html" "css" "scss" "javascript"]
   :highlight {:enable true
               :disable ["scss" "javascript"]} 
   :indent  {:enable true
             :disable ["scss"]}})
