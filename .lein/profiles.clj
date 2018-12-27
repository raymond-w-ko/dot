{:user {:plugins [[lein-cljfmt "0.6.3"]
                  [lein-ancient "0.6.15"]
                  [lein-kibit "0.1.6"]]
        :dependencies [[org.clojure/clojure "1.10.0"]
                       [slamhound "1.5.5"]
                       [cljfmt "0.6.3"]]
        :repl-options {:init (require 'cljfmt.core)}
        :aliases {"slamhound" ["run" "-m" "slam.hound"]}
        }}
