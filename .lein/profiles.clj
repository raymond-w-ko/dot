{:user {:plugins [[cider/cider-nrepl "0.8.2"]
                  [lein-cljfmt "0.1.10"]
                  [venantius/ultra "0.3.3"]]
        :dependencies [[cljfmt "0.1.10"]
                       [slamhound "RELEASE"]]
        :aliases {"slamhound" ["run" "-m" "slam.hound"]}
        :ultra {:color-scheme :solarized_dark}}}
