(module config.plugin.bufdelete
  {autoload {nvim aniseed.nvim
             core aniseed.core}})

(nvim.set_keymap :n :<delete> ":Bdelete<cr>" {})
