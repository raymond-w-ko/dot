{:user {:plugins [[lein-cljfmt "0.6.1"]
                  [venantius/ultra "0.5.2"]
                  [lein-ancient "0.6.15"]
                  [lein-kibit "0.1.6"]
                  [cider/cider-nrepl "0.18.0"]]
        :dependencies [[slamhound "1.5.5"]
                       [cljfmt "0.6.1"]]
        :repl-options {:init (require 'cljfmt.core)}
        :aliases {"slamhound" ["run" "-m" "slam.hound"]}
        :ultra {:color-scheme :solarized_dark}} }
