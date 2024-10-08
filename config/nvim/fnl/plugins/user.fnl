(local uu (require :dotfiles.util))

[(uu.tx :Olical/nfnl {:priority 9001 :ft ["fennel"]})
 (uu.tx :bakpakin/fennel.vim)
 (uu.tx :shortcuts/no-neck-pain.nvim)

 (uu.tx :loganswartz/selenized.nvim
        {:dependencies [:rktjmp/lush.nvim]
         :config (fn []
                   (tset vim.g :selenized_variant "normal")
                   (tset vim.o :background "light")
                   (vim.cmd "colorscheme selenized"))})

 (uu.tx "windwp/nvim-autopairs"
        {:event "InsertEnter"
         :config true})

 (uu.tx :nvim-telescope/telescope.nvim
        {:dependencies [:nvim-lua/plenary.nvim]
         :keys [["<leader>ff" "<cmd>Telescope find_files<cr>"]]
         :tag "0.1.8"})]
