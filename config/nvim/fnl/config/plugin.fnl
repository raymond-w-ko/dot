(module config.plugin
  {autoload {nvim aniseed.nvim
             a aniseed.core
             packer packer}})

(defn- safe-require-plugin-config [name]
  (let [(ok? val-or-err) (pcall require (.. :config.plugin. name))]
    (when (not ok?)
      (print (.. "config error: " val-or-err)))))

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
;;
(use
  ; plugin manager
  :wbthomason/packer.nvim {}
  ; nvim config and plugins in Fennel
  :Olical/aniseed {:branch :develop}
  :Olical/conjure {:branch :master
                   :mod :conjure}

  :ellisonleao/gruvbox.nvim {:requires [:rktjmp/lush.nvim]
                             :mod :theme}
  :kana/vim-arpeggio {:mod :arpeggio}
  :nvim-treesitter/nvim-treesitter {:run ":TSUpdate"
                                    :mod :treesitter}
  :nvim-treesitter/nvim-treesitter-textobjects {}
  :neovim/nvim-lspconfig  {:mod :lspconfig}

  :nvim-telescope/telescope.nvim {:requires [:nvim-lua/popup.nvim
                                             :nvim-lua/plenary.nvim]
                                  :mod :telescope}
  :gennaro-tedesco/nvim-peekup {:mod :peekup}
  :tpope/vim-repeat {}
  :tpope/vim-surround {}
  :guns/vim-sexp {:mod :sexp}
  :tpope/vim-sexp-mappings-for-regular-people {}

  :tpope/vim-fugitive  {:mod :fugitive}
  ;:sunjon/Shade.nvim {:mod :shade}
  :norcalli/nvim-colorizer.lua {:mod :colorizer}
  :famiu/bufdelete.nvim {:mod :bufdelete}
  :ggandor/lightspeed.nvim {:mod :lightspeed}

  :roxma/vim-tmux-clipboard {}
  :christoomey/vim-tmux-navigator {:mod :tmux-navigator})
