{:user {:plugins [[lein-cljfmt "0.7.0"]
                  [lein-kibit "0.1.8"]
                  [jonase/eastwood "0.3.11"]]
        :dependencies [[org.clojure/clojure "1.10.1"]
                       [cljfmt "0.7.0"]]
        :repl-options {:init (require 'cljfmt.core)}
        :aliases {}
        }}
