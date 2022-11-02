(module rko.plugin.flit
  {autoload {nvim aniseed.nvim
             core aniseed.core
             flit flit}})

(flit.setup {:keys {:f "f"
                    :F "F"
                    :t "t"
                    :T "T"}
             :labeled_modes "v"
             :multiline true
             :opts {}})
