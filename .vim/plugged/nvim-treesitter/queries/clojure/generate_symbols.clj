#!/bin/sh
#_(

   #_DEPS is same format as deps.edn. Multiline is okay.
   DEPS='
   {:deps {}}
   '

   #_You can put other options here
   OPTS='
   -J-Xms1024m -J-Xmx1024m
   '

exec clojure $OPTS -Sdeps "$DEPS" "$0" "$@"

)

(in-ns 'clojure.core)
(require '[clojure.pprint :as cp])
(require '[clojure.string :as str])

(defn is-symbol-a-fn? [s]
  (fn? @(resolve s)))

(defn lua-regex-escape [s]
  (str "\"" s "\""))

(defn spit-symbols [file coll]
  (->> (sort coll)
       (map name)
       (map lua-regex-escape)
       (interpose " ")
       (apply str)
       (spit file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def macros (keep (fn [[s v]]
                    (when (and (:macro (meta v))
                               (is-symbol-a-fn? s))
                      s))
                  (ns-publics *ns*)))

(def functions (keep (fn [[s v]]
                       (when (and (not (:macro (meta v)))
                                  (is-symbol-a-fn? s))
                         s))
                     (ns-publics *ns*)))

(def defs (keep (fn [[s v]]
                  (let [sym-name (name s)]
                    (when (and (or (str/starts-with? sym-name "def")
                                   (contains? #{"declare" "ns"} sym-name))
                               (not (contains? #{"default-data-readers" "defs"} sym-name)))
                      s)))
                (ns-publics *ns*)))

(spit-symbols "clojure.core.functions.txt" functions)
(spit-symbols "clojure.core.macros.txt" macros)
(spit-symbols "clojure.core.defs.txt" defs)
