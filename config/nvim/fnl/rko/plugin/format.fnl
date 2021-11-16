(module rko.plugin.format
  {autoload {nvim aniseed.nvim
             format format}})

(format.setup
  {:* [{:cmd ["sed -i 's/[ \t]*$//'"]}]
   :javascript [{:cmd ["prettier -w"]}]
   :html [{:cmd ["prettier --parser html -w"]}]})
