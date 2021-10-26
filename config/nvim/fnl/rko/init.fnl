(module rko.init
  {autoload {core aniseed.core
             nvim aniseed.nvim
             nu aniseed.nvim.util
             str aniseed.string
             utils rko.utils}})

(set nvim.g.mapleader " ")
(set nvim.g.maplocalleader ",")
(set nvim.g.loaded_matchparen 1)

(let [options ["undofile" "noswapfile"
               "splitbelow" "splitright"
               "autowrite" "autowriteall"
               "relativenumber"
               "noemoji"]]
  (each [i x (ipairs options)]
    (nvim.ex.set x)))

(let [options
      {:completeopt "menu,menuone,noselect,noinsert"
       :wildmode "longest:full"
       :smartcase true
       :ignorecase true
       :showtabline 2
       :expandtab true
       :shiftwidth 2
       :tabstop 2
       :softtabstop 2
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
; (nvim.set_keymap :n :<leader>l ":nohlsearch<cr>" {})
(nvim.set_keymap :n :<tab> ":tabprev<cr>" {})
(nvim.set_keymap :n :\ ":tabnext<cr>" {})
(nvim.set_keymap :n :<S-Left> "<C-w>R" {})
(nvim.set_keymap :n :<S-Right> "<C-w>r" {})

(let [text-objects [[:r "["]
                    [:f "("]
                    [:c "{"]
                    [:q "\""]
                    [:s "\""]]]
  (each [i [a b] (ipairs text-objects)]
    (nvim.set_keymap :o (.. "i" a) (.. "i" b) {:noremap true})
    (nvim.set_keymap :o (.. "a" a) (.. "a" b) {:noremap true})
    (nvim.set_keymap :v (.. "i" a) (.. "i" b) {:noremap true})
    (nvim.set_keymap :v (.. "a" a) (.. "a" b) {:noremap true})))

(utils.multi-line-nvim-cmd
  "augroup rko_init
  au!
  au CursorHold * checktime
  augroup END")

(require :rko.plugin)
(require :rko.fns)

(nvim.set_keymap :n :<leader>tt ":call Rko_create_tab_splits()<cr>" {})
