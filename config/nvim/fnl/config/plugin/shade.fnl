(module config.plugin.shade
  {autoload {nvim aniseed.nvim
             shade shade}})

(shade.setup {:overlay_opacity 50
              :opacity_step 1})
