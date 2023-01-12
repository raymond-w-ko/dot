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

(set nvim.g.clojure_align_subforms 1)

(use
  ; plugin manager
  :wbthomason/packer.nvim {}
  ; nvim config and plugins in Fennel
  :Olical/aniseed {:branch :develop}
  :Olical/conjure {:branch :develop :mod :conjure}

  ;; vim compatible
  :kana/vim-arpeggio {:mod :arpeggio}
  :justinmk/vim-dirvish {:mod :dirvish}
  :raymond-w-ko/vim-asterisk {:mod :asterisk}
  :clojure-vim/clojure.vim {}
  :romainl/vim-qf {:mod :qf}
  :tikhomirov/vim-glsl {}

  ;; :rhysd/vim-color-spring-night {:mod :theme}
  :ellisonleao/gruvbox.nvim {:requires [:rktjmp/lush.nvim] :mod :theme}
  :hoob3rt/lualine.nvim {:mod :lualine}

  :windwp/nvim-autopairs {:mod :autopairs}
  :windwp/nvim-ts-autotag {:mod :autotag}

  :nvim-treesitter/nvim-treesitter {:run ":TSUpdate"
                                    :mod :treesitter}
  :nvim-treesitter/nvim-treesitter-textobjects {}
  :nvim-treesitter/playground {}

  :kyazdani42/nvim-web-devicons {}
  :nvim-telescope/telescope.nvim {:requires [:nvim-lua/popup.nvim
                                             :nvim-lua/plenary.nvim]
                                  :branch :master
                                  :mod :telescope}
  :RishabhRD/lspactions {:mod :lspactions}
  :nvim-telescope/telescope-fzf-native.nvim {:run "make"}
  :folke/trouble.nvim {:mod :trouble}
  :gennaro-tedesco/nvim-peekup {:mod :peekup}
  :tpope/vim-repeat {}
  :tpope/vim-surround {}
  :tpope/vim-obsession {}
  :tpope/vim-characterize {}
  :tpope/vim-unimpaired {}
  ;; lisp
  ; :guns/vim-sexp {:mod :sexp}
  ; :tpope/vim-sexp-mappings-for-regular-people {}
  ; :gpanders/nvim-parinfer {}
  ; :eraserhd/parinfer-rust {}

  ; :tpope/vim-fugitive  {:mod :fugitive}
  ;; :kdheepak/lazygit.nvim {:mod :lazygit :branch "main"}
  :norcalli/nvim-colorizer.lua {:mod :colorizer}
  :famiu/bufdelete.nvim {:mod :bufdelete}
  ;; :ggandor/lightspeed.nvim {:mod :lightspeed}
  :ggandor/leap.nvim {:mod :leap}
  :ggandor/flit.nvim {:mod :flit}
  :numToStr/Comment.nvim {:mod :comment}
  ; :lukas-reineke/format.nvim {:mod :format}
  :mhartington/formatter.nvim {:mod :formatter}
  :lewis6991/gitsigns.nvim {:mod :gitsigns}
  :akinsho/toggleterm.nvim {:mod :toggleterm :branch "main"}
  :monkoose/matchparen.nvim {:mod :matchparen}

  :neovim/nvim-lspconfig {:mod :lspconfig}
  "~/src/vim-geckocomplete" {:mod :geckocomplete}
  "~/src/nvim-sexp-edit" {}

  ;; hmm
  ; :ms-jpq/coq_nvim {:branch "coq" :mod :coq}
  ; :ms-jpq/coq.artifacts {:branch "artifacts"}
  
  ;; slightly slow
  ; :yamatsum/nvim-cursorline {:mod :cursorline}

  ;; insufficient
  ; :rktjmp/highlight-current-n.nvim {:mod :highlight}

  ;; distracting
  ; :kevinhwang91/nvim-hlslens {:mod :hlslens}

  ;; broken
  ; :sunjon/Shade.nvim {:mod :shade}
  ; :rmagatti/auto-session {}

  :roxma/vim-tmux-clipboard {}
  :christoomey/vim-tmux-navigator {:mod :tmux-navigator})
