(module config.init
  {autoload {core aniseed.core
             nvim aniseed.nvim
             nu aniseed.nvim.util
             str aniseed.string}})

(set nvim.g.mapleader " ")
(set nvim.g.maplocalleader ",")

(let [options ["undofile"
               "splitbelow" "splitright"
               "relativenumber"]]
  (each [i x (ipairs options)]
    (nvim.ex.set x)))

(let [options
      {:completeopt "menu,menuone,noselect,noinsert"
       :smartcase true
       :ignorecase true
       :sessionoptions "blank,buffers,curdir,folds,help,tabpages,winsize,resize,winpos,terminal"
       :signcolumn "number"
       :pastetoggle "<f9>"}]
  (each [k v (pairs options)]
    (core.assoc nvim.o k v)))

(nvim.set_keymap :n :H "^" {})
(nvim.set_keymap :n :L "g_" {})
(nvim.set_keymap :n :<cr> ":w<cr>" {})
(nvim.set_keymap :n :<leader><leader> "<c-^>" {})
(nvim.set_keymap :n :<leader>l ":nohlsearch<cr>" {})

(require :config.plugin)
(require :config.fns)

(nvim.set_keymap :n :<leader>tt ":call Rko_create_tab_splits()<cr>" {})
