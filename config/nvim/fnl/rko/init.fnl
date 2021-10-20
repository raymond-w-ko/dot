(module rko.init
  {autoload {core aniseed.core
             nvim aniseed.nvim
             nu aniseed.nvim.util
             str aniseed.string}})

(set nvim.g.mapleader " ")
(set nvim.g.maplocalleader ",")
(set nvim.g.loaded_matchparen 1)

(let [options ["undofile"
               "splitbelow" "splitright"
               "autowrite" "autowriteall"
               "relativenumber"]]
  (each [i x (ipairs options)]
    (nvim.ex.set x)))

(let [options
      {:completeopt "menu,menuone,noselect,noinsert"
       :smartcase true
       :ignorecase true
       :showtabline 2
       :sessionoptions "blank,buffers,curdir,folds,help,tabpages,winsize,resize,winpos,terminal"
       :signcolumn "number"
       :clipboard ""
       :pastetoggle "<f9>"}]
  (each [k v (pairs options)]
    (core.assoc nvim.o k v)))

(nvim.set_keymap :n :H "^" {})
(nvim.set_keymap :n :L "g_" {})
(nvim.set_keymap :n :<cr> ":w<cr>" {})
(nvim.set_keymap :n :<leader><leader> "<c-^>" {})
(nvim.set_keymap :n :<leader>l ":nohlsearch<cr>" {})
(nvim.set_keymap :n :<tab> ":tabprev<cr>" {})
(nvim.set_keymap :n :\ ":tabnext<cr>" {})
(nvim.set_keymap :n :<S-Left> "<C-w>R" {})
(nvim.set_keymap :n :<S-Right> "<C-w>r" {})

(require :rko.plugin)
(require :rko.fns)

(nvim.set_keymap :n :<leader>tt ":call Rko_create_tab_splits()<cr>" {})
