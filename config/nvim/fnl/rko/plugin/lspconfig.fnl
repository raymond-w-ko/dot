(module rko.plugin.lspconfig
  {autoload {nvim aniseed.nvim
             lspc lspconfig
             coq coq}})

(let [handlers
      {"textDocument/publishDiagnostics"
       (vim.lsp.with
         vim.lsp.diagnostic.on_publish_diagnostics
         {:severity_sort true
          :update_in_insert false
          :underline true
          :virtual_text true})
       "textDocument/hover"
       (vim.lsp.with
         vim.lsp.handlers.hover
         {:border "single"})
       "textDocument/signatureHelp"
       (vim.lsp.with
         vim.lsp.handlers.signature_help
         {:border "single"})}
      capabilities
      (vim.lsp.protocol.make_client_capabilities)
      on_attach
      (fn [client bufnr]
        (do
          (nvim.buf_set_keymap bufnr :n :gd "<Cmd>lua vim.lsp.buf.definition()<CR>" {:noremap true})
          (nvim.buf_set_keymap bufnr :n :<leader>f "<Cmd>lua vim.lsp.buf.formatting()<CR>" {:noremap true})))]
  (lspc.clojure_lsp.setup (-> {:on_attach on_attach
                               :handlers handlers
                               :capabilities capabilities}
                              (coq.lsp_ensure_capabilities))))
