(module rko.plugin.hlslens
  {autoload {nvim aniseed.nvim
             core aniseed.core}})

(nvim.set_keymap "" :n "<Cmd>execute('normal! ' . v:count1 . 'n')<CR><Cmd>lua require('hlslens').start()<CR>"
                 {:noremap true :silent true})
(nvim.set_keymap "" :N "<Cmd>execute('normal! ' . v:count1 . 'N')<CR><Cmd>lua require('hlslens').start()<CR>"
                 {:noremap true :silent true})
(nvim.set_keymap "" :* "*<Cmd>lua require('hlslens').start()<CR>" {:noremap true})
(nvim.set_keymap "" :# "#<Cmd>lua require('hlslens').start()<CR>" {:noremap true})
(nvim.set_keymap "" :g* "g*<Cmd>lua require('hlslens').start()<CR>" {:noremap true})
(nvim.set_keymap "" :g# "g#<Cmd>lua require('hlslens').start()<CR>" {:noremap true})
