(module rko.plugin.format
  {autoload {nvim aniseed.nvim
             format format}})

(format.setup
  {:* [{:cmd ["sed -i 's/[ \t]*$//'"]}]
   :javascript [{:cmd ["prettier -w"]}]
   :cpp [{:cmd ["clang-format -i"]}]
   :html [{:cmd ["prettier --parser html -w"]}]})

(nvim.set_keymap :n :<leader>f ":Format<cr>" {})
