(module config.plugin.lualine
  {autoload {nvim aniseed.nvim
             core aniseed.core
             lualine lualine}})

(defn lsp-connection []
  (if (vim.tbl_isempty (vim.lsp.buf_get_clients 0))
    ""
    "[LSP]"))

(lualine.setup
  {:options {:theme "gruvbox_light"
             :icons_enabled true}
   :sections {:lualine_a [{1 :mode
                           :lower true}]
              :lualine_b []
              :lualine_c [["FugitiveHead"]
                          {1 :filename
                           :file_status true :path 1}]
              :lualine_x [{1 :diagnostics
                           :sections [:error :warn :info :hint]
                           :sources [:nvim_lsp]}
                          [lsp-connection]
                          :location
                          :filetype]
              :lualine_y [:encoding]
              :lualine_z []}})
