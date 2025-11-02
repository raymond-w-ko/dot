(ns gen
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn safe-keyword [x]
  (cond
    (keyword? x) x
    (number? x) (keyword (str x))
    (symbol? x) (let [ns* (namespace x)]
                  (if (not= ns* "at")
                    (keyword (name x))
                    (keyword x)))
    :else (keyword x)))

(defmacro ->hash [xs]  
  `(->> ~xs
        (map safe-keyword)
        (partition-all 2)
        (map vec)
        (into {})))

(defmacro tap-hold*
  ([alias-name hold-time x y]
   (let [hold-time* (keyword (case hold-time
                               :normal '$hold-time
                               :slow '$hold-time-slow))]
     `[~alias-name (list :tap-hold :$tap-time ~hold-time* ~x ~y)]))
  ([hold-time x y]
   (let [hold-time* (keyword (case hold-time
                               :normal '$hold-time
                               :slow '$hold-time-slow))]
     `[~x (list :tap-hold :$tap-time ~hold-time* ~x ~y)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ->symbols [xs]
  (mapv safe-keyword xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *env (atom nil))
(defn is-laptop? []
  (assert @*env)
  (str/includes? @*env "laptop"))

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

(defn copy []
  (cond
    (str/starts-with? @*env "macos.") :M-c
    (str/starts-with? @*env "windows.") :C-c
    :else nop))

(defn paste []
  (cond
    (str/starts-with? @*env "macos.") :M-v
    (str/starts-with? @*env "windows.") :C-v
    :else nop))

(defn screenshot-area []
  (cond
    (str/starts-with? @*env "macos.") :C-M-S-4
    (str/starts-with? @*env "windows.") :M-S-s
    :else nop))

(defn next-tab [] :C-tab)
(defn previous-tab [] :C-S-tab)

(defn outdent-line []
  (cond
    (str/starts-with? @*env "macos.") :M-lbrc
    (str/starts-with? @*env "windows.") :C-lbrc
    :else nop))

(defn indent-line []
  (cond
    (str/starts-with? @*env "macos.") :M-rbrc
    (str/starts-with? @*env "windows.") :C-rbrc
    :else nop))

(defn cycle-app-windows []
  (cond
    (str/starts-with? @*env "macos.") :M-grv
    (str/starts-with? @*env "windows.") :M-grv ;; there is no equivalent for M-grv on Windows
    :else nop))

(defn start-of-paragraph [] :A-lbrc)
(defn end-of-paragraph [] :A-rbrc)
(defn select-to-start-of-paragraph [] :A-S-lbrc)
(defn select-to-end-of-paragraph [] :A-S-rbrc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn gen-aliases-data []
  (assert @*env)
  `[:l_shortcut (layer-toggle shortcut)
    :l_sym1 (layer-toggle sym1)
    :l_misc (layer-toggle misc)
    :l_num (layer-toggle num)

    ~@(tap-hold* :normal :f :at/l_shortcut)
    ~@(tap-hold* :normal :j :at/l_shortcut)
    ~@(tap-hold* :normal :d :at/l_sym1)
    ~@(tap-hold* :normal :k :at/l_sym1)
    ~@(tap-hold* :normal :s :at/l_misc)
    ~@(tap-hold* :normal :l :at/l_misc)
    ~@(tap-hold* :slow :a :at/l_num)
    ~@(tap-hold* :slow :scln :at/l_num)
    
    ~@(tap-hold* :slow :q :lsft)
    ~@(tap-hold* :slow :w :lctl)
    ~@(tap-hold* :normal :e :lalt)
    ~@(tap-hold* :normal :r :lmet)
    
    ~@(tap-hold* :normal :p :lsft)
    ~@(tap-hold* :slow :o :lctl)
    ~@(tap-hold* :normal :i :lalt)
    ~@(tap-hold* :normal :u :lmet)
    
    ~@(tap-hold* :d_h :normal :del :home)
    ~@(tap-hold* :p_e :normal (outdent-line) :end)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def fn-to-action-keys
  (->hash '[f1 brup f2 brdown
            f6 lrld
            f7 prev f8 pp f9 next
            f10 mute f11 vold f12 volu]))

(def qwerty-to-base-layer
  (->hash '[q :at/q w :at/w e :at/e r :at/r
            u :at/u o :at/o i :at/i p :at/p

            a :at/a s :at/s d :at/d f :at/f
            j :at/j k :at/k l :at/l scln :at/scln]))

(defn gen-qwerty-to-shortcut-layer []
  (->hash `[i up
            j left
            k down
            l right
            o bspc
            scln ret

            u ~(copy)
            p ~(paste)

            q tab
            w esc
            e ~(previous-tab)
            r ~(next-tab)
            a ~(cycle-app-windows)
            s :at/d_h
            d :at/p_e
            f ~(indent-line)
            g ~(screenshot-area)]))

(defn gen-qwerty-to-misc-layer []
  (->hash `[e ~(end-of-paragraph)
            r ~(select-to-end-of-paragraph)
            d ~(start-of-paragraph)
            f ~(select-to-start-of-paragraph)
            v ~(copy)
            t ~(paste)]))

(def qwerty-to-symbol-layer
  (->hash '[q S-1 ;; !
            w XX  ;; TODO
            e S-3 ;; #
            r S-4 ;; $
            t S-5 ;; %
            y S-6 ;; ^
            u S-7 ;; &
            i S-8 ;; *
            o S-9 ;; (
            p S-0 ;; )
            a grv ;; `
            s S-2 ;; @
            d -   ;; -
            f bksl    ;; \
            g S-bksl  ;; |
            ;; h
            j S-scln  ;; :
            k scln    ;; ;
            l lbrc    ;; [
            scln rbrc ;; ]

            m S-=    ;; +
            comm =   ;; =
            . S-lbrc ;; {
            / S-rbrc ;; }
            ]))

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

(defn align-spaces [n]
  (apply str (repeat (max 1 (- 6 n)) " ")))

(defn append-form [buffer form]
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

(defn gen-vars [{:as args :keys []}]
  (let [env @*env
        form `(defvar tap-time ~(case env
                                  "macos.laptop" 200
                                  "windows.alice" 200
                                  200)
                hold-time ~(case env
                             "macos.laptop" 200
                             "windows.alice" 200
                             200)
                hold-time-slow ~(case env
                                  "macos.laptop" 225
                                  "windows.alice" 225
                                  225))]
    (update args :buffer append-form form)))

(defn gen-aliases [{:as args :keys []}]
  (let [form `(defalias ~@(gen-aliases-data))]
    (update args :buffer append-form form)))

(defn gen-src-keys [{:as args :keys []}]
  (let [form (reduce (fn [acc key]
                       (cond
                         (and (= key :fn) (not (is-laptop?))) acc
                         :else (conj acc key)))
                     [] src-keys)
        form `(defsrc ~@form)]
    (update args :buffer append-form form)))

(defn compute-dst-key [layer src-key]
  (case layer
    :base (cond
            (contains? fn-to-action-keys src-key) (get fn-to-action-keys src-key)
            (contains? qwerty-to-base-layer src-key) (get qwerty-to-base-layer src-key)
            :else src-key)
    :shortcut (let [m (gen-qwerty-to-shortcut-layer)]
                (cond
                  (contains? m src-key) (get m src-key)
                  :else nop))
    :sym1 (cond
            (contains? qwerty-to-symbol-layer src-key) (get qwerty-to-symbol-layer src-key)
            :else nop)
    :misc (let [m (gen-qwerty-to-misc-layer)]
            (cond
              (contains? m src-key) (get m src-key)
              :else nop))
    :num (cond
           (contains? qwerty-to-numbers src-key) (get qwerty-to-numbers src-key)
           :else nop)
    ;; else
    nop))

(defn gen-layer [{:as args :keys []} layer]
  (let [rf (fn [acc src-key]
             (cond
               (and (= src-key :fn) (not (is-laptop?))) acc
               :else (let [dst-key (compute-dst-key layer src-key)
                           dst-key (if (symbol? dst-key)
                                     (with-meta dst-key {:comment (str src-key)})
                                     dst-key)]
                       (into acc [dst-key]))))
        form (reduce rf [] src-keys)
        form `(deflayer ~layer ~@form)]
    ;; (println form)
    (update args :buffer append-form form)))

(defn gen-cfg [{:as args :keys []}]
  (assert @*env)
  (-> args
      (update :buffer append-form '(defcfg process-unmapped-keys no))
      (gen-vars)
      (gen-aliases)
      (gen-src-keys)
      (gen-layer :base)
      (gen-layer :shortcut)
      (gen-layer :sym1)
      (gen-layer :misc)
      (gen-layer :num)))

(defn write-kbd [{:as args :keys [buffer]}]
  (with-open [f (io/writer (str @*env ".kbd"))]
    (doseq [chunk buffer]
      (.write f chunk)))
  args)

(reset! *env "windows.alice")
(-> {:buffer []}
    (gen-cfg)
    (write-kbd))

(reset! *env "macos.laptop")
(-> {:buffer []}
    (gen-cfg)
    (write-kbd))
