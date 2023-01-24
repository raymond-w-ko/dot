(module rko.plugin.toggleterm
  {autoload {nvim aniseed.nvim
             toggleterm toggleterm
             toggletermterminal toggleterm.terminal}})

(def Terminal toggletermterminal.Terminal)

(toggleterm.setup
  {:open_mapping "<C-\\>"
   :winblend 0
   :shade_terminals false
   :highlights {}
   :direction :float
   :float_opts {:border :double}})

(def shell
  (Terminal:new
    {:cmd "zsh"
     :open_mapping "<F5>"
     :hidden true
     :direction "float"
     :on_open (fn [term]
                (vim.cmd "startinsert!")
                (vim.keymap.set "t" "<F5>" "<cmd>close<CR>"
                                {:buffer term.bufnr :noremap true :silent true}))
     :on_close (fn [term]
                 (vim.cmd "startinsert!"))}))
(vim.keymap.set :n "<F5>" (fn [] (shell:toggle)) {:silent true})

(def lazygit (Terminal:new {:cmd "lazygit"
                            :close_on_exit true
                            :hidden true
                            :direction "float"}))
(vim.keymap.set :n "<leader>g" (fn [] (lazygit:toggle)) {:silent true})
