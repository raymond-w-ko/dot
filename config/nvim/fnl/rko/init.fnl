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
               "number" "norelativenumber"
               "noemoji"
               "wrap" "breakindent"]]
  (each [i x (ipairs options)]
    (nvim.ex.set x)))

(let [options
      {:timeoutlen 3000
       :completeopt "menu,menuone,noselect,noinsert"
       :wildmode "longest:full"
       :smartcase true
       :ignorecase true
       :showtabline 2
       :expandtab true
       :winwidth (+ 88 4)
       :shiftwidth 2
       :tabstop 2
       :softtabstop 2
       :sessionoptions "blank,buffers,curdir,folds,help,tabpages,winsize,resize,winpos"
       :signcolumn "number"
       :clipboard ""
       :shortmess "aIcF"
       :pastetoggle "<f9>"}]
  (each [k v (pairs options)]
    (core.assoc nvim.o k v)))

(vim.keymap.set :n "H" "^")
(vim.keymap.set :n "L" "g_")
(vim.keymap.set :n "<cr>" ":w<cr>")
(vim.keymap.set :n "<leader><leader>" "<c-^>" {})
(vim.keymap.set :n :<leader>l ":nohlsearch<cr>" {})

;; (vim.keymap.set :n "<tab>" ":tabprev<cr>" {})
;; (vim.keymap.set :n "\\" ":tabnext<cr>" {})
(vim.keymap.set :n "<f3>" ":tabprev<cr>" {})
(vim.keymap.set :n "<f1>" ":tabnext<cr>" {})

(vim.keymap.set :v "Q" "gq" {})
(vim.keymap.set :n "<S-Left>"  ":call Rko_mark_window_swap_src()<cr><c-w>h:call Rko_do_window_swap()<cr>" {})
(vim.keymap.set :n "<S-Right>" ":call Rko_mark_window_swap_src()<cr><c-w>l:call Rko_do_window_swap()<cr>" {})
(vim.keymap.set :c "w!!" "w !sudo tee > /dev/null %")
(vim.keymap.set :n "R" ":e<cr>zz")
(vim.keymap.set :n "j" "gj")
(vim.keymap.set :n "k" "gk")

(let [text-objects [[:r "["]
                    [:f "("]
                    [:c "{"]
                    [:q "\""]
                    [:s "\""]]]
  (each [i [a b] (ipairs text-objects)]
    (comment nvim.set_keymap :o (.. "i" a) (.. "i" b) {:noremap true})
    (comment nvim.set_keymap :o (.. "a" a) (.. "a" b) {:noremap true})
    (comment nvim.set_keymap :v (.. "i" a) (.. "i" b) {:noremap true})
    (comment nvim.set_keymap :v (.. "a" a) (.. "a" b) {:noremap true})))

(utils.multi-line-nvim-cmd
  "augroup rko_init
  au!
  au CursorHold * checktime
  augroup END")

(require :rko.plugin)
(require :rko.fns)

(nvim.set_keymap :n :<leader>tt ":call Rko_create_tab_splits()<cr>" {})

(let [ex vim.api.nvim_command]
  (ex "augroup rko_init_fnl")
  (ex "autocmd!")
  (ex "autocmd InsertEnter * set nolist")
  (ex "autocmd InsertLeave * set list")
  (ex "autocmd FileType php,javascript setl iskeyword+=$")
  (ex "autocmd FileType css,scss setl iskeyword+=-")
  (ex "autocmd FileType scss setl iskeyword+=$")
  (ex "autocmd FocusGained * checktime")
  (ex "autocmd VimEnter,WinEnter,BufWinEnter * setlocal cursorline")
  (ex "autocmd WinLeave * setlocal nocursorline")
  (ex "augroup END"))
