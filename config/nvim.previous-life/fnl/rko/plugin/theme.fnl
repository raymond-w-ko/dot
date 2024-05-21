(module rko.plugin.theme
  {autoload {core aniseed.core
             nvim aniseed.nvim
             gruvbox gruvbox}})

(nvim.ex.set :termguicolors)

(defn apply-gruvbox []
  (set nvim.o.background "light")

  ;; (set nvim.g.gruvbox_contrast_light "soft")
  (gruvbox.setup
    {:underline false
     :undercurl false
     :strikethrough false

     :invert_selections true
     :bold true
     :italic {:strings false
              :operators false
              :comments false}
     :inverse true
     :invert_signs false
     :invert_tabline false
     :invert_intend_guides false

     :contrast "soft"})
  (nvim.ex.colorscheme "gruvbox")
  nil)

(defn apply-spring-night []
  (set nvim.o.background "dark")
  (set nvim.g.spring_night_kill_italic 1)
  (nvim.ex.colorscheme "spring-night"))

;; (apply-spring-night)
(apply-gruvbox)

(vim.api.nvim_command "hi MatchParen guifg=#000000 guibg=#00ff00")
;; (vim.api.nvim_command "hi Pmenu guifg=#aaaaaa guibg=#222222")
