(ns geckocomplete.complete
  (:import
   [geckocomplete Algorithm])
  (:require
   [clojure.test :as test :refer [deftest is]]
   [com.climate.claypoole :as cp]
   [taoensso.timbre :as timbre
    :refer [log  trace  debug  info  warn  error  fatal  report
            logf tracef debugf infof warnf errorf fatalf reportf
            spy get-env]]
   ; [taoensso.tufte :as tufte :refer [defnp p profiled profile]]
   [clj-async-profiler.core :as prof :refer [profile]]
   
   [geckocomplete.macros :refer [->hash cond-xlet]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce _0 (Algorithm/init))
(defonce cpu-pool (cp/threadpool (cp/ncpus)))
(defonce cpu-pool-shutdown-hook
  (.. Runtime
      (getRuntime)
      (addShutdownHook (new Thread (fn [] (cp/shutdown cpu-pool))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-min-length? [word]
  (< 3 (count word)))

(defn is-same-length? [needle word]
  (= (count needle) (count word)))

(defn format-word [word]
  {:word word :score -1.0})

(defn is-zero-score? [{:keys [score]}]
  ;; scores are from negative infinity to zero
  (>= score 0.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-subsequence? [haystack needle]
  (Algorithm/isSubSequence haystack needle))

(deftest is-subsequence?-test
  (is (false? (is-subsequence? "a" "abc")))
  (is (true? (is-subsequence? "abc" "abc")))
  (is (true? (is-subsequence? "StartRemoteServer" "srs")))
  (is (true? (is-subsequence? "StartRemoteServer" "temver")))
  (is (true? (is-subsequence? "StartRemoteServer" "StartRemoteServer")))
  (is (false? (is-subsequence? "StartRemoteServer" "StartRemoteServerb")))
  (is (false? (is-subsequence? "StartRemoteServer" "temverb")))
  (is (false? (is-subsequence? "StartRemoteServer" "srb")))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn word-boundaries [{:as m :keys [word]}]
  (let [wb (Algorithm/wordBoundaries word)]
    (assoc m :word-boundaries wb)))

(comment (Algorithm/wordBoundaries "foo-bar"))

(deftest word-boundaries-test
  (letfn [(f [word]
            (-> {:word word}
                (word-boundaries)
                :word-boundaries))]
    (is (= nil (f "foo")))
    (is (= nil (f "food")))
    (is (= "sc" (f "snake_case")))
    (is (= "nc" (f "needle-case")))
    (is (= "lc" (f "lisp-case")))
    (is (= "cc" (f "camelCase")))
    (is (= "osw" (f "OutputStreamWriter")))
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn clumping-score ^double [^String haystack ^String needle]
  (let [raw-score (Algorithm/allCommonSubstringSimilarityScore haystack needle)]
    (/ raw-score (-> needle count double))))

(deftest clumping-score-test
  (is (<= 0.78 (clumping-score "eatsleepnightxyz" "eatsleepabcxyz"))))

(comment
  (clumping-score "compute-clumping-score" "comcc")
  (clumping-score "compute-clumping-score" "ccscore"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn compute-score [needle {:as m :keys [word word-boundaries]}]
  (let [n-need (double (count needle))
        n-word (double (count word))
        is-subseq-of-word (is-subsequence? word needle)
        is-subseq-of-wb (if word-boundaries
                          (is-subsequence? word-boundaries needle)
                          false)]
    (assoc m :score
           (- 0.0
              ;; % of 'word' that shares a common subsequence with needle.
              (if is-subseq-of-word
                (/ n-need n-word)
                0.0)

              ;; % of 'word-boundaries' that shares a common subsequence with
              ;; needle
              (if is-subseq-of-wb
                (/ n-need (-> word-boundaries count double))
                0.0)

              ;; add clumping score if needle share common subsequence
              ;; with 'word' and 'word-boundaries'
              (if (and is-subseq-of-word is-subseq-of-wb)
                (clumping-score word needle)
                0.0)))))

(defn score-word-using-needle [needle]
  (comp (remove (partial is-same-length? needle))
        (map format-word)
        (map word-boundaries)
        (map (partial compute-score needle))
        (remove is-zero-score?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sort-words [words]
  (sort-by (fn [{:keys [score word]}] [score word]) words))

(defn format-completion [{:keys [word]} index]
  {"word" word
   "abbr" (str (if (= 10 index) "0 " (str index " "))
               word)})

(defn chunked-pmap [partition-size f coll]
  (->> coll
       (partition-all partition-size)
       (pmap (comp doall
                   (partial map f)))
       (apply concat)))               

(defn complete-parallel
  "WARN: This exhibits huge variance in latency."
  [{:keys [word-set]} needle]
  (cond-xlet
   :let [xf (score-word-using-needle needle)
         f (fn [chunk*]
             (into [] xf chunk*))
         words (->> (partition-all (cp/ncpus) word-set)
                    (cp/upmap cpu-pool f)
                    (apply concat))]
   :return
   (->> (sort-words words)
        (take 10)
        (vec))))

(defn complete-sync [{:keys [word-set]} needle]
  (cond-xlet
   :let [xf (score-word-using-needle needle)]
   :return
   (->> (into [] xf word-set)
        (sort-words)
        (take 10)
        (vec))))

(defonce *last-args (atom nil))

(defn complete [{:as args :keys []} needle]
  (reset! *last-args args)
  ; (debug "word-set count:" (count word-set))
  (cond-xlet
   :let [completions (complete-parallel args needle)]

   ;; we are only matching ourselves, abort!
   (and (= 1 (count completions))
        (= needle (-> completions first :word)))
   []

   :return (mapv format-completion completions (range 1 11))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (profile (dotimes [i 100] (complete @*last-args "genf"))))
