{:user {:plugins [[lein-cljfmt "0.6.8"]
                  [lein-kibit "0.1.8"]
                  [jonase/eastwood "0.3.11"]]
        :dependencies [[org.clojure/clojure "1.10.1"]
                       [slamhound "1.5.5"]
                       [cljfmt "0.6.8"]]
        :repl-options {:init (require 'cljfmt.core)}
        :aliases {"slamhound" ["run" "-m" "slam.hound"]}
        }}
