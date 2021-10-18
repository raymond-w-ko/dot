(module config.plugin
  {autoload {nvim aniseed.nvim
             a aniseed.core
             packer packer}})

(defn- safe-require-plugin-config [name]
  (let [(ok? val-or-err) (pcall require (.. :config.plugin. name))]
    (when (not ok?)
      (print (.. "config error: " val-or-err)))))

(defn- use [...]
  "Iterates through the arguments as pairs and calls packer's use function for
  each of them. Works around Fennel not liking mixed associative and sequential
  tables as well."
  (let [pkgs [...]]
    (packer.startup
      (fn [use]
        (for [i 1 (a.count pkgs) 2]
          (let [name (. pkgs i)
                opts (. pkgs (+ i 1))]
            (-?> (. opts :mod) (safe-require-plugin-config))
            (use (a.assoc opts 1 name)))))))
  nil)

(use
 ; plugin manager
 :wbthomason/packer.nvim {}
 ; nvim config and plugins in Fennel
 :Olical/aniseed {:branch :develop}
 :Olical/conjure {:branch :master
                  :mod :conjure}

 :ellisonleao/gruvbox.nvim {:requires [:rktjmp/lush.nvim]
                            :mod :theme}
 )
