(module config.plugin.lspconfig
  {autoload {nvim aniseed.nvim
             lspc lspconfig}})

(let [handlers {}
      capabilities {}
      on_attach (fn [client bufnr]
                  nil)]
  (lspc.clojure_lsp.setup {:on_attach on_attach
                           :handlers handlers
                           :capabilities capabilities}))
