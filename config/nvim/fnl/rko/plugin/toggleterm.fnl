(module rko.plugin.toggleterm
  {autoload {nvim aniseed.nvim
             toggleterm toggleterm}})

(toggleterm.setup
  {:open_mapping "<F4>"
   :winblend 0
   :shade_terminals false
   :highlights {}
   :direction :tab
   :float_opts {:border :double}})
