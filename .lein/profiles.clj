{:user {:plugins [[cider/cider-nrepl "0.9.1"]
                  [lein-cljfmt "0.3.0"]
                  [venantius/ultra "0.3.4"]
                  [lein-ancient "0.6.7"]]
        :dependencies [[slamhound "RELEASE"]]
        :aliases {"slamhound" ["run" "-m" "slam.hound"]}
        :ultra {:color-scheme :solarized_dark}}}
