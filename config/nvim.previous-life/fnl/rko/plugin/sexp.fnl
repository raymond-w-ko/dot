(module rko.plugin.sexp
  {autoload {nvim aniseed.nvim}})
(set nvim.g.sexp_filetypes "clojure,scheme,lisp,timl,fennel")
;; disables swap element colides with tmux sizing
(set nvim.g.sexp_mappings {:sexp_swap_element_backward ""
                           :sexp_swap_element_forward ""
                           :sexp_swap_list_backward ""
                           :sexp_swap_list_forward ""})
