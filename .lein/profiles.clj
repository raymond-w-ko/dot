{:user {:plugins [[refactor-nrepl  "1.2.0"]
                  [cider/cider-nrepl "0.9.1"]
                  [lein-cljfmt "0.3.0"]
                  [venantius/ultra "0.4.0"]
                  [lein-ancient "0.6.8"]]
        :dependencies [[slamhound "1.5.5"]]
        :aliases {"slamhound" ["run" "-m" "slam.hound"]}
        :ultra {:color-scheme :solarized_dark}}}
