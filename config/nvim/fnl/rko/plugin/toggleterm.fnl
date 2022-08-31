(module rko.plugin.toggleterm
  {autoload {nvim aniseed.nvim
             toggleterm toggleterm
             toggletermterminal toggleterm.terminal}})

(def Terminal toggletermterminal.Terminal)

(toggleterm.setup
  {:open_mapping "<F4>"
   :winblend 0
   :shade_terminals false
   :highlights {}
   :direction :tab
   :float_opts {:border :double}})

(def lazygit (Terminal:new {:cmd "lazygit"
                            :hidden true
                            :direction "float"}))
(vim.keymap.set :n "<leader>g" (fn [] (lazygit:toggle)) {:silent true})
