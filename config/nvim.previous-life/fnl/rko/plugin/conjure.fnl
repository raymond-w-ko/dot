(module rko.plugin.conjure
  {autoload {core aniseed.core
             nvim aniseed.nvim
             str aniseed.string
             utils rko.utils}})

(set nvim.g.conjure#client#fennel#aniseed#aniseed_module_prefix
     "aniseed.")

(set nvim.g.conjure#client#conjure#nrepl#auto_require false)
(set nvim.g.conjure#client#conjure#nrepl#connection#auto_repl#enabled false)

;; (utils.multi-line-nvim-cmd
;;   "augroup rko_auto_conjure
;;   au!
;;   au BufWritePost *.clj ConjureEvalFile
;;   augroup END")
