(module rko.plugin.coq
  {autoload {nvim aniseed.nvim}})

(set nvim.g.coq_settings
     {:clients {:tree_sitter {:enabled false}
                :tmux {:enabled false}
                :tags {:enabled false}
                :buffers {:enabled false
                          :same_filetype true}}})
