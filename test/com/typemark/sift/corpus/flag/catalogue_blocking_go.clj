(ns corpus.flag.catalogue-blocking-go
  (:require [clojure.core.async :as a]))
(defn start [c result]
  (a/go (a/alts!! [c (a/timeout 1000)]) (a/close! result)))
