(module rko.utils
  {autoload {core aniseed.core
             nvim aniseed.nvim
             str aniseed.string}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn multi-line-nvim-cmd [s]
  (->> (str.split s "\n")
       (core.map str.trim)
       (core.map nvim.command)))
