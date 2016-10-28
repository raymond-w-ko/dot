{:user {:plugins [[refactor-nrepl  "2.2.0"]
                  [cider/cider-nrepl "0.14.0"]
                  [lein-cljfmt "0.5.6"]
                  [venantius/ultra "0.5.0"]
                  [lein-ancient "0.6.10"]
                  [lein-kibit "0.1.2"]]
        :dependencies [[slamhound "1.5.5"]
                       [cljfmt "0.5.6"]]
        :repl-options {:init (require 'cljfmt.core)}
        :aliases {"slamhound" ["run" "-m" "slam.hound"]}
        :ultra {:color-scheme :solarized_dark}} }
