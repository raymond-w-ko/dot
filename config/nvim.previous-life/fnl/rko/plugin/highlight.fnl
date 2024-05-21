(module rko.plugin.highlight
  {autoload {nvim aniseed.nvim
             hcn highlight_current_n
             utils rko.utils}})

(hcn.setup
  {})

(nvim.set_keymap :n :n "<plug>(highlight-current-n-n)" {})
(nvim.set_keymap :n :N "<plug>(highlight-current-n-N)" {})
(nvim.set_keymap :n :* "*N" {})

(utils.multi-line-nvim-cmd
  "
  augroup ClearSearchHL
  autocmd!
  autocmd CmdlineEnter /,\\? set hlsearch
  autocmd CmdlineLeave /,\\? set nohlsearch
  autocmd CmdlineLeave /,\\? lua require('highlight_current_n')['/,?']()
  augroup END
  ")
