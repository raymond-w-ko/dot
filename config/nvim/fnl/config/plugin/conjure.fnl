(module config.plugin.conjure
  {autoload {nvim aniseed.nvim}})

(set nvim.g.conjure#mapping#doc_word "K")

(set nvim.g.conjure#client#fennel#aniseed#aniseed_module_prefix "aniseed.") ; attempt to index global 'nvim' (a nil value)

(set nvim.g.conjure#client#conjure#nrepl#auto_reuquire false)
(set nvim.g.conjure#client#conjure#nrepl#connection#auto_repl#enabled false)
