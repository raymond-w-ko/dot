(module rko.plugin.telescope
  {autoload {nvim aniseed.nvim
             nu aniseed.nvim.util
             telescope telescope
             tb telescope.builtin}})

(telescope.setup
  {:defaults {:file_ignore_patterns ["node_modules"
                                     "package-lock.json"]}
   :pickers {:find_files {:find_command ["rg" "--files" "--iglob" "!.git" "--hidden"]}}})

(defn live-grep []
  (tb.live_grep {:additional_args (fn []
                                    ["-g" "!package-lock.json"
                                     "-g" "!*.min.js"
                                     "-g" "!*.map"])}))
(nu.fn-bridge :Rko_telescope_live_grep :rko.plugin.telescope :live-grep)

(defn- bind [x y]
  (nvim.set_keymap :n x y {:noremap true}))
(bind :<C-p> ":lua require('telescope.builtin').find_files()<CR>")
(bind :<leader>b ":lua require('telescope.builtin').buffers()<CR>")
(bind :<leader>r ":call Rko_telescope_live_grep()<CR>")
