(module rko.plugin.theme
  {autoload {core aniseed.core
             nvim aniseed.nvim
             gruvbox gruvbox}})

(nvim.ex.set :termguicolors)
(set nvim.o.background "light")
;; (set nvim.g.gruvbox_contrast_light "soft")
(gruvbox.setup
  {:underline false
   :undercurl false
   :strikethrough false

   :invert_selections true
   :bold true
   :italic false
   :inverse true
   :invert_signs false
   :invert_tabline false
   :invert_intend_guides false

   :contrast "soft"})
(nvim.ex.colorscheme "gruvbox")

(vim.api.nvim_command "hi MatchParen guifg=#000000 guibg=#00ff00")
;; (vim.api.nvim_command "hi Pmenu guifg=#aaaaaa guibg=#222222")
