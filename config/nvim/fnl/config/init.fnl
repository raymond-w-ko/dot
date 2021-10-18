(module config.init
  {autoload {core aniseed.core
             nvim aniseed.nvim
             str aniseed.string}})

(set nvim.g.mapleader " ")
(set nvim.g.maplocalleader ",")

(nvim.ex.set "undofile")
(nvim.ex.set "splitbelow")
(nvim.ex.set "splitright")

(let [options
      {:completeopt "menu,menuone,noselect,noinsert"
       :smartcase true
       :ignorecase true}]
  (each [k v (pairs options)]
    (core.assoc nvim.o k v)))

(nvim.set_keymap :n :H "^" {})
(nvim.set_keymap :n :L "g_" {})
(nvim.set_keymap :n :<cr> ":w<cr>" {})
(nvim.set_keymap :n :<leader><leader> "<c-^>" {})
(nvim.set_keymap :n :<leader>l ":nohlsearch<cr>" {})

(require :config.plugin)
