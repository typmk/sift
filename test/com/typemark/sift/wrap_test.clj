(ns com.typemark.sift.wrap-test
  "The wrapper's half of the trace contract: what wrap writes, defnet's
   ingest/trace.cljs must read. The key vocabulary asserted here is copied
   from that file's docstring; a key outside it is a contract break."
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [com.typemark.sift.wrap :as w]))

(defn add-rate [x y] (+ x y))
(defn compose [x] (add-rate x 2.0))
(defn blows-up [] (throw (ex-info "boom" {})))

(defn- trace-lines [f]
  (->> (slurp f)
       str/split-lines
       (remove str/blank?)
       (mapv #(json/read-str % :key-fn keyword))))

(deftest wrap-writes-defnet-trace-lines
  (let [f (java.io.File/createTempFile "trace" ".jsonl")]
    (.delete f)
    (w/start! (.getPath f))
    (w/wrap-var! #'add-rate)
    (w/wrap-var! #'compose)
    (try
      (compose 1.5)
      (finally (w/stop!)))
    (let [lines (trace-lines f)
          inner (first (filter #(= "com.typemark.sift.wrap-test/add-rate" (:subject %)) lines))
          outer (first (filter #(= "com.typemark.sift.wrap-test/compose" (:subject %)) lines))]
      (is (= 2 (count lines)))
      (is (some? inner))
      (is (some? outer))
      (is (= ["com.typemark.sift.wrap-test/compose"
              "com.typemark.sift.wrap-test/add-rate"]
             (:call inner))
          "the caller comes off the stack, through the wrapper on compose")
      (is (= [{:slot 0 :type "Double"} {:slot 1 :type "Double"}] (:args inner)))
      (is (= "Double" (:ret inner)))
      (is (number? (:ms inner)))
      (is (string? (:ctx inner)))
      (doseq [l lines k (keys l)]
        (is (contains? #{:call :subject :args :ret :count :ms :metrics :far-end :ctx} k)
            (str k " is not in defnet's trace vocabulary")))
      (.delete f))))

(deftest a-throw-still-writes-a-line
  (let [f (java.io.File/createTempFile "trace" ".jsonl")]
    (.delete f)
    (w/start! (.getPath f))
    (w/wrap-var! #'blows-up)
    (try
      (is (thrown? clojure.lang.ExceptionInfo (blows-up)))
      (finally (w/stop!)))
    (let [[l] (trace-lines f)]
      (is (= "com.typemark.sift.wrap-test/blows-up" (:subject l)))
      (is (= {:throws 1} (:metrics l)))
      (is (nil? (:ret l)) "no return type — it did not return")
      (.delete f))))

(deftest unwrap-restores-the-original
  (let [f (java.io.File/createTempFile "trace" ".jsonl")]
    (.delete f)
    (w/start! (.getPath f))
    (w/wrap-var! #'add-rate)
    (w/stop!)
    (add-rate 1 2)
    (is (empty? (trace-lines f)) "an unwrapped call writes nothing")
    (.delete f)))
