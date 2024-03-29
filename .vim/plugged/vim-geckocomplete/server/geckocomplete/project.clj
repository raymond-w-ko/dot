(defproject geckocomplete "0.1.0-SNAPSHOT"
  :description "Server process for geckocomplete"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :jvm-opts ["-Djdk.attach.allowAttachSelf"
             "-XX:+UnlockDiagnosticVMOptions"
             "-XX:+DebugNonSafepoints"
             "-XX:-OmitStackTraceInFastThrow"]
  :dependencies [[org.clojure/clojure "1.10.3"]]
  :main ^:skip-aot geckocomplete.core
  :target-path "target/%s"
  :plugins [[lein-tools-deps "0.4.5"]
            [lein-virgil "0.1.9"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :java-source-paths ["src/java"]
  :lein-tools-deps/config {:config-files [:project]}
  :javac-options ["-target" "1.8" "-source" "1.8" "-Xlint:-options"]
  :profiles {:uberjar {:aot :all}})
