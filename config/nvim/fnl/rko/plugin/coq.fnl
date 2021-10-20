(module rko.plugin.lspconfig
  {autoload {nvim aniseed.nvim
             coq coq}})

(set nvim.g.coq_settings
     {:clients {:tree_sitter {:enabled false}
                :tmux {:enabled false}
                :tags {:enabled false}
                :buffers {:enabled true
                          :same_filetype true}}})
