(local uu (require :dotfiles.util))

[(uu.tx :Olical/nfnl {:priority 9001 :ft ["fennel"]})
 (uu.tx :bakpakin/fennel.vim)
 ;; conflicts with nmap S
 ; (uu.tx :shortcuts/no-neck-pain.nvim)

 (uu.tx :loganswartz/selenized.nvim
        {:dependencies [:rktjmp/lush.nvim]
         :config (fn []
                   (tset _G.vim.g :selenized_variant "normal")
                   (tset _G.vim.o :background "light")
                   (vim.cmd "colorscheme selenized"))})

 ;; does not work by default for Lisp languages
 ; (uu.tx "windwp/nvim-autopairs" {:event "InsertEnter" :config true})
 (uu.tx "echasnovski/mini.pairs" {:version false
                                  :config (fn []
                                            (local MiniPairs (require "mini.pairs"))
                                            (MiniPairs.setup))})

 (uu.tx :nvim-telescope/telescope.nvim
        {:dependencies [:nvim-lua/plenary.nvim]
         :keys [["<leader>ff" "<cmd>Telescope find_files<cr>"]]
         :tag "0.1.8"})
 
 (uu.tx :ggandor/leap.nvim
        {:dependencies [:tpope/vim-repeat]
         :config (fn []
                   (local leap (require "leap"))
                   (leap.create_default_mappings))})
 
 (uu.tx :ggandor/flit.nvim
        {:dependencies [:ggandor/leap.nvim]
         :config (fn []
                   (local flit (require "flit"))
                   (flit.setup))})

 (uu.tx :nvim-lualine/lualine.nvim
        {:dependencies [:loganswartz/selenized.nvim
                        :nvim-tree/nvim-web-devicons]
         :config (fn []
                   (local lualine (require "lualine"))
                   (lualine.setup {:theme "selenized"}))})
 ]


