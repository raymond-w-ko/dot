{:user {:plugins [[refactor-nrepl  "2.0.0"]
                  [cider/cider-nrepl "0.10.2"]
                  [lein-cljfmt "0.3.0"]
                  [venantius/ultra "0.4.0"]
                  [lein-ancient "0.6.8"]
                  [lein-kibit "0.1.2"]]
        :dependencies [[slamhound "1.5.5"]
                       [cljfmt "0.3.0"]]
        :repl-options {:init (require 'cljfmt.core)}
        :aliases {"slamhound" ["run" "-m" "slam.hound"]}
        :ultra {:color-scheme :solarized_dark}} }
