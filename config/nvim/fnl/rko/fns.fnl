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
