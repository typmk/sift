(ns data-rules
  (:require [clojure.core.async :as a]))

;; only what splint does not say — see rules.edn's header
(defn g [xs] (first (filter even? xs)))
(defn j [] (Thread/sleep 100))
(defn m [a x] (swap! a (fn [v] (conj v (count @a) x))))

;; host interop as data — full class names resolve without an import
(defn r [in] (java.io.ObjectInputStream. in))
(defn t [] (java.io.File/createTempFile "a" "b"))
(defn p [c] (java.lang.ProcessBuilder. c))
(defn look [n] (javax.naming.InitialContext/doLookup n))

;; ---- the Clojure smell catalogue, one firing each -------------------------
(defn blocking [c] (a/go (a/alts!! [c (a/timeout 1)])))
(defn nested [] (atom {:history (atom [])}))
(defn orient [x] (case x "p" :portrait "l" :landscape :else :default))
(defn internals [xs] (iterator-seq (clojure.lang.RT/iter xs)))
(defprotocol IMarker)
(defmulti ^:private dispatch-on (fn [x] x))
(in-ns 'data-rules)
(require '[clojure.set :as s])
