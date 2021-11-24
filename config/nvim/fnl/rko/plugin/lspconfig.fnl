(module rko.plugin.lspconfig
  {autoload {nvim aniseed.nvim
             lspc lspconfig}})

(defn on_attach [client bufnr]
  (let [map (fn [a b]
              (nvim.buf_set_keymap bufnr :n a b {:noremap true}))]
    (map :gd "<Cmd>lua vim.lsp.buf.definition()<CR>")
    (map :<leader>f "<Cmd>lua vim.lsp.buf.formatting()<CR>")))

(let [handlers
      {"textDocument/publishDiagnostics" (vim.lsp.with
                                           vim.lsp.diagnostic.on_publish_diagnostics
                                           {:severity_sort true
                                            :update_in_insert false
                                            :underline true
                                            :virtual_text true})
       "textDocument/hover" (vim.lsp.with
                              vim.lsp.handlers.hover
                              {:border "single"})
       "textDocument/signatureHelp" (vim.lsp.with
                                      vim.lsp.handlers.signature_help
                                      {:border "single"})}
      capabilities (vim.lsp.protocol.make_client_capabilities)
      on_attach on_attach]
  (lspc.clojure_lsp.setup (-> {:on_attach on_attach
                               :handlers handlers
                               :capabilities capabilities}))
  ; (lspc.tsserver.setup (-> {:on_attach on_attach
  ;                           :handlers handlers
  ;                           :capabilities capabilities}))
  
  (lspc.eslint.setup (-> {:on_attach on_attach
                          :handlers handlers
                          :capabilities capabilities})))
