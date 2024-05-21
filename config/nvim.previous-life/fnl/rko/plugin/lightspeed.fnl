(module rko.plugin.lightspeed
  {autoload {nvim aniseed.nvim
             core aniseed.core
             lightspeed lightspeed}})

(lightspeed.setup {})

(nvim.ex.hi "LightspeedMaskedChar guifg=#994444")
(nvim.ex.hi "LightspeedGreyWash guifg=#999999")
(nvim.ex.hi "LightspeedLabelDistant guifg=#076678")
(nvim.ex.hi "LightspeedLabelDistantOverlapped guifg=#458588")
