(module rko.fns
  {autoload {core aniseed.core
             nvim aniseed.nvim
             nu aniseed.nvim.util
             str aniseed.string}})

(defn create-vsplits []
  (let [num-tabs (length (nvim.list_tabpages))]
    (when (< 1 num-tabs)
      (nvim.ex.tabnew)))
  
  (let [num-splits (-> nvim.o.columns
                       (/ (- 80 1))
                       (- 1)
                       (math.floor))]
    (for [i 1 num-splits]
      (nvim.ex.vnew))))
(nu.fn-bridge :Rko_create_tab_splits :rko.fns :create-vsplits)
