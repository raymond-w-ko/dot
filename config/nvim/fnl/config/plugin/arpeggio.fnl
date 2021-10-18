(module config.plugin.arpeggio
  {autoload {nvim aniseed.nvim}})

(set nvim.g.arpeggio_timeoutlen 50)
(nvim.command "call arpeggio#map('i', '', 0, 'jk', '<esc>')")
