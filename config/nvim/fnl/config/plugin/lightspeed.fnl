(module config.plugin.lightspeed
  {autoload {nvim aniseed.nvim
             core aniseed.core
             lightspeed lightspeed}})

(lightspeed.setup {})

(nvim.command "hi LightspeedMaskedChar guifg=#994444")
(nvim.command "hi LightspeedGreyWash guifg=#999999")
(nvim.command "hi LightspeedLabelDistant guifg=#076678")
(nvim.command "hi LightspeedLabelDistantOverlapped guifg=#458588")
