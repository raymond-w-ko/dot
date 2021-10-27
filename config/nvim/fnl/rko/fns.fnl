(module rko.fns
  {autoload {core aniseed.core
             nvim aniseed.nvim
             nu aniseed.nvim.util
             str aniseed.string}})

(defn create-vsplits []
  (let [num-tabs (length (nvim.list_tabpages))
        num-wins (length (nvim.list_wins))]
    (nvim.echo num-wins)
    (when (or (< 1 num-tabs)
              (< 1 num-wins))
      (nvim.ex.tabnew)))
  
  (let [num-splits (-> nvim.o.columns
                       (/ (- 80 1))
                       (- 1)
                       (math.floor))]
    (for [i 1 num-splits]
      (nvim.ex.vnew))
    (for [i 1 (- num-splits 1)]
      (nvim.ex.wincmd "h"))))
(nu.fn-bridge :Rko_create_tab_splits :rko.fns :create-vsplits)

(var src-window-swap-num nil)

(defn mark-window-swap-src []
  (set src-window-swap-num (nvim.fn.winnr)))

(defn do-window-swap []
  (let [win (nvim.fn.winnr)
        buf (nvim.fn.bufnr "%")]
    (nvim.command (.. src-window-swap-num "wincmd w"))
    (let [marked-buf (nvim.fn.bufnr "%")]
      (nvim.command (.. "hide buf " buf))
      (nvim.command (.. win "wincmd w"))
      (nvim.command (.. "hide buf " marked-buf)))))
      
(nu.fn-bridge :Rko_mark_window_swap_src :rko.fns :mark-window-swap-src)
(nu.fn-bridge :Rko_do_window_swap :rko.fns :do-window-swap)
