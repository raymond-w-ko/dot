(module config.plugin.theme
  {autoload {core aniseed.core
             nvim aniseed.nvim}})

(nvim.ex.set :termguicolors)
(set nvim.o.background "light")
(set nvim.g.gruvbox_contrast_light "soft")
(nvim.command "colorscheme gruvbox")
