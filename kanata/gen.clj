(ns gen
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn safe-keyword [x]
  (cond
    (keyword? x) x
    (number? x) (keyword (str x))
    :else (keyword x)))

(defmacro ->hash [xs]
  `(->> ~xs
        (map safe-keyword)
        (partition-all 2)
        (map vec)
        (into {})))

(defn ->symbols [xs]
  (mapv safe-keyword xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def nop (keyword "•"))

(def src-keys
  "These are keys that might get remapped to different keys.
   At the very least, they are intercepted by kanata."
  (->symbols '[f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12
               tab q w e r t y u i o p lbrc rbrc bksl
               grv 1 2 3 4 5 6 7 8 9 0 - = bspc
               caps a s d f g h j k l scln apos ret
               lsft z x c v b n m comm . / rsft
               fn lctl lalt lmet spc rmet ralt up down left right]))

(def fn-to-action-keys
  (->hash '[f1 brup f2 brdown
            f7 prev f8 pp f9 next
            f10 mute f11 vold f12 volu]))

(def qwerty-to-base-layer
  (->hash '[w :at/w e :at/e r :at/r
            u :at/u o :at/o i :at/i
            
            a :at/a s :at/s d :at/d f :at/f
            j :at/j k :at/k l :at/l scln :at/scln]))

(def qwerty-to-cmd-layer
  (->hash '[i up
            j left
            k down
            l right
            
            scln ret
            o bspc
            q tab]))

(def qwerty-to-numbers
  (->hash '[m 1
            comm 2
            . 3
            j 4
            k 5
            l 6
            u 7
            i 8
            o 9
            p 0
            scln kp.]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro tap-hold* [hold-time x y]
  (let [hold-time* (keyword (case hold-time
                              :normal '$hold-time
                              :slow '$hold-time-slow))]
    `[~x (list :tap-hold :$tap-time ~hold-time* ~x ~y)]))

(def aliases
  `[:l_cmd (layer-toggle cmd)
    :l_sys1 (layer-toggle sys1)
    :l_sys2 (layer-toggle sys2)
    :l_num (layer-toggle num)

    ~@(tap-hold* :normal :f :at/l_cmd)
    ~@(tap-hold* :normal :j :at/l_cmd)
    ~@(tap-hold* :normal :d :at/l_sys1)
    ~@(tap-hold* :normal :k :at/l_sys1)
    ~@(tap-hold* :normal :s :at/l_sys2)
    ~@(tap-hold* :normal :l :at/l_sys2)
    ~@(tap-hold* :slow :a :at/l_num)
    ~@(tap-hold* :slow :scln :at/l_num)

    ~@(tap-hold* :slow :w :lctl)
    ~@(tap-hold* :normal :e :lalt)
    ~@(tap-hold* :normal :r :lmet)

    ~@(tap-hold* :slow :o :lctl)
    ~@(tap-hold* :normal :i :lalt)
    ~@(tap-hold* :normal :u :lmet)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn align-spaces [n]
  (apply str (repeat (max 1 (- 6 n)) " ")))

(defn append-form [env buffer form]
  (let [*lines (atom [])
        write-comment! (fn [x]
                         (let [{:keys [comment]} (meta x)]
                           (when comment
                             (swap! *lines conj (str (align-spaces (count (name x)))
                                                     ";; " comment
                                                     "\n")))))
        write-symbol! (fn [level x*]
                        (let [x (name x*)
                              y (namespace x*)]
                          (when y
                            (case y
                              "at" (swap! *lines conj "@")
                              nil))
                          (swap! *lines conj (name x))
                          (swap! *lines conj " ")
                          (when (str/starts-with? x "def")
                            (swap! *lines conj "\n"))
                          (write-comment! x)))
        f (fn f [level x]
            (cond
              (seq? x) (do (swap! *lines conj "(")
                           (dorun (map (partial f (inc level)) x))
                           (swap! *lines conj ")")
                           (swap! *lines conj (if (zero? level) "\n\n" "\n")))
              (keyword? x) (write-symbol! level x)
              (symbol? x) (write-symbol! level x)
              :else (do (swap! *lines conj (pr-str x))
                        (swap! *lines conj " "))))]
    (f 0 form)
    (into buffer @*lines)))

(defn gen-vars [{:as args :keys [env]}]
  (let [form `(defvar tap-time ~(case env
                                  "macos.laptop" 200
                                  "windows.alice" 200
                                  200)
                hold-time ~(case env
                             "macos.laptop" 200
                             "windows.alice" 200
                             200)
                hold-time-slow ~(case env
                                  "macos.laptop" 300
                                  "windows.alice" 300
                                  300))]
    (update args :buffer (partial append-form env) form)))

(defn gen-aliases [{:as args :keys [env]}]
  (let [form `(defalias ~@aliases)]
    (update args :buffer (partial append-form env) form)))

(defn gen-src-keys [{:as args :keys [env]}]
  (let [form (reduce (fn [acc key]
                       (cond
                         (and (= key :fn) (not= env "macos.laptop")) acc
                         :else (conj acc key)))
                     [] src-keys)
        form `(defsrc ~@form)]
    (update args :buffer (partial append-form env) form)))

(defn compute-dst-key [env layer src-key]
  (case layer
    :base (cond
            (contains? qwerty-to-base-layer src-key) (get qwerty-to-base-layer src-key)
            :else src-key)
    :cmd (cond
           (contains? qwerty-to-cmd-layer src-key) (get qwerty-to-cmd-layer src-key)
           :else nop)
    :num (cond
           (contains? qwerty-to-numbers src-key) (get qwerty-to-numbers src-key)
           :else nop)
    ;; else
    nop))

(defn gen-layer [{:as args :keys [env]} layer]
  (let [rf (fn [acc src-key]
             (cond
               (and (= src-key :fn) (not= env "macos.laptop")) acc
               :else (let [dst-key (compute-dst-key env layer src-key)
                           dst-key (if (symbol? dst-key)
                                     (with-meta dst-key {:comment (str src-key)})
                                     dst-key)]
                       (into acc [dst-key]))))
        form (reduce rf [] src-keys)
        form `(deflayer ~layer ~@form)]
    (println form)
    (update args :buffer (partial append-form env) form)))

(defn gen-cfg [{:as args :keys [env]}]
  (-> args
      (update :buffer (partial append-form env) '(defcfg process-unmapped-keys no))
      (gen-vars)
      (gen-aliases)
      (gen-src-keys)
      (gen-layer :base)
      (gen-layer :cmd)
      (gen-layer :sys1)
      (gen-layer :sys2)
      (gen-layer :num)))

(defn write-kbd [{:as args :keys [buffer env]}]
  (with-open [f (io/writer (str env ".kbd"))]
    (doseq [chunk buffer]
      (.write f chunk)))
  args)

(-> {:buffer [] :env "windows.alice"}
    (gen-cfg)
    (write-kbd))

(-> {:buffer [] :env "macos.laptop"}
    (gen-cfg)
    (write-kbd))
