(module rko.plugin.asterisk
  {autoload {nvim aniseed.nvim}})

(set nvim.g.asterisk#keeppos 1)

(nvim.set_keymap "" :* "<plug>(asterisk-z*)" {})
(nvim.set_keymap "" :# "<plug>(asterisk-z#)" {})
(nvim.set_keymap "" :g* "<plug>(asterisk-gz*)" {})
(nvim.set_keymap "" :g# "<plug>(asterisk-gz#)" {})
