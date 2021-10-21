(module rko.plugin.asterisk
  {autoload {nvim aniseed.nvim}})

(set nvim.g.asterisk#keeppos 1)

(nvim.set_keymap :n :* "<plug>(asterisk-z*)" {})
(nvim.set_keymap :n :# "<plug>(asterisk-z#)" {})
(nvim.set_keymap :n :g* "<plug>(asterisk-gz*)" {})
(nvim.set_keymap :n :g# "<plug>(asterisk-gz#)" {})
