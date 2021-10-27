(module rko.plugin
  {autoload {nvim aniseed.nvim
             a aniseed.core
             packer packer}})

(defn- safe-require-plugin-config [name]
  (let [(ok? val-or-err) (pcall require (.. :rko.plugin. name))]
    (when (not ok?)
      (print (.. "plugin on load mod error: " val-or-err)))))

(defn- use [...]
  "Iterates through the arguments as pairs and calls packer's use function for
  each of them. Works around Fennel not liking mixed associative and sequential
  tables as well."
  (let [pkgs [...]]
    (packer.startup
      (fn [use]
        (for [i 1 (a.count pkgs) 2]
          (let [name (. pkgs i)
                opts (. pkgs (+ i 1))]
            (-?> (. opts :mod) (safe-require-plugin-config))
            (use (a.assoc opts 1 name)))))))
  nil)

(use
  ; plugin manager
  :wbthomason/packer.nvim {}
  ; nvim config and plugins in Fennel
  :Olical/aniseed {:branch :develop}
  :Olical/conjure {:branch :master
                   :mod :conjure}

  ;; vim compatible
  :kana/vim-arpeggio {:mod :arpeggio}
  :justinmk/vim-dirvish {:mod :dirvish}
  :raymond-w-ko/vim-asterisk {:mod :asterisk}

  :ellisonleao/gruvbox.nvim {:requires [:rktjmp/lush.nvim]
                             :mod :theme}
  :hoob3rt/lualine.nvim {:mod :lualine}

  :windwp/nvim-autopairs {:mod :autopairs}
  :windwp/nvim-ts-autotag {:mod :autotag}

  :nvim-treesitter/nvim-treesitter {:run ":TSUpdate"
                                    :mod :treesitter}
  :nvim-treesitter/nvim-treesitter-textobjects {}

  :kyazdani42/nvim-web-devicons {}
  :nvim-telescope/telescope.nvim {:requires [:nvim-lua/popup.nvim
                                             :nvim-lua/plenary.nvim]
                                  :mod :telescope}
  :nvim-telescope/telescope-fzf-native.nvim {:run "make"}
  :folke/trouble.nvim {:mod :trouble}
  :gennaro-tedesco/nvim-peekup {:mod :peekup}
  :tpope/vim-repeat {}
  :tpope/vim-surround {}
  :tpope/vim-obsession {}
  :tpope/vim-characterize {}
  ;; lisp
  ; :guns/vim-sexp {:mod :sexp}
  ; :tpope/vim-sexp-mappings-for-regular-people {}
  ; :gpanders/nvim-parinfer {}
  :eraserhd/parinfer-rust {}

  :tpope/vim-fugitive  {:mod :fugitive}
  :norcalli/nvim-colorizer.lua {:mod :colorizer}
  :famiu/bufdelete.nvim {:mod :bufdelete}
  :ggandor/lightspeed.nvim {:mod :lightspeed}
  :yamatsum/nvim-cursorline {:mod :cursorline}
  :numToStr/Comment.nvim {:mod :comment}
  :lewis6991/gitsigns.nvim {:mod :gitsigns}

  ; :ms-jpq/coq_nvim {:branch "coq" :mod :coq}
  ; :ms-jpq/coq.artifacts {:branch "artifacts"}
  :neovim/nvim-lspconfig {:mod :lspconfig}

  ;; insufficient
  ; :rktjmp/highlight-current-n.nvim {:mod :highlight}

  ;; distracting
  ; :kevinhwang91/nvim-hlslens {:mod :hlslens}

  ;; broken
  ; :sunjon/Shade.nvim {:mod :shade}
  ; :rmagatti/auto-session {}

  :roxma/vim-tmux-clipboard {}
  :christoomey/vim-tmux-navigator {:mod :tmux-navigator})
