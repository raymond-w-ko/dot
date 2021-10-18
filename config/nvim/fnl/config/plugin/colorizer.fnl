(module config.plugin.colorizer
  {autoload {nvim aniseed.nvim
             colorizer colorizer}})

(colorizer.setup ["html" "css"])
