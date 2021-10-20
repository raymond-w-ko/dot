(module rko.plugin.highlight
  {autoload {nvim aniseed.nvim
             hcn highlight_current_n}})

(hcn.setup
  {})

(nvim.set_keymap :n :n "<plug>(highlight-current-n-n)" {})
(nvim.set_keymap :n :N "<plug>(highlight-current-n-N)" {})
(nvim.set_keymap :n :* "*N" {})

(nvim.command "augroup ClearSearchHL")
(nvim.command "autocmd!")
(nvim.command "autocmd CmdlineEnter /,\\? set hlsearch")
(nvim.command "autocmd CmdlineLeave /,\\? set nohlsearch")
(nvim.command "autocmd CmdlineLeave /,\\? lua require('highlight_current_n')['/,?']()")
(nvim.command "augroup END")
