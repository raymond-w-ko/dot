(module rko.plugin.telescope
  {autoload {nvim aniseed.nvim
             telescope telescope}})
(telescope.setup {:defaults {:file_ignore_patterns ["node_modules"]}
                  :pickers {:find_files {:find_command ["rg" "--files" "--iglob" "!.git" "--hidden"]}}})

(nvim.set_keymap :n :<C-p> ":lua require('telescope.builtin').find_files()<CR>" {:noremap true})
(nvim.set_keymap :n :<leader>b ":lua require('telescope.builtin').buffers()<CR>" {:noremap true})
(nvim.set_keymap :n :<leader>fg ":lua require('telescope.builtin').live_grep()<CR>" {:noremap true})
