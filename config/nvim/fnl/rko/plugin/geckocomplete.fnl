(module rko.plugin.geckocomplete
  {autoload {nvim aniseed.nvim}})

(defn- mapi [a b]
  (nvim.set_keymap :i a b {:silent true :expr true :nowait true}))

(mapi :<C-s> "geckocomplete#toggle_pause_completion()")
(mapi :<Tab> "geckocomplete#completion_key()")
