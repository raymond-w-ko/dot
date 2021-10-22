(module rko.plugin.conjure
  {autoload {nvim aniseed.nvim}})

(set nvim.g.conjure#mapping#doc_word "K")

(set nvim.g.conjure#client#fennel#aniseed#aniseed_module_prefix
     "aniseed.")

(set nvim.g.conjure#client#conjure#nrepl#auto_require false)
(set nvim.g.conjure#client#conjure#nrepl#connection#auto_repl#enabled false)
