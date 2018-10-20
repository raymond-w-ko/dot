{:user {:plugins [[lein-cljfmt "0.6.1"]
                  [lein-ancient "0.6.15"]
                  [lein-kibit "0.1.6"]]
        :dependencies [[slamhound "1.5.5"]
                       [cljfmt "0.6.1"]]
        :repl-options {:init (require 'cljfmt.core)}
        :aliases {"slamhound" ["run" "-m" "slam.hound"]}
        }}
