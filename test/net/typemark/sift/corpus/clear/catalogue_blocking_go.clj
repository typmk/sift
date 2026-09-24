(ns corpus.clear.catalogue-blocking-go
  (:require [clojure.core.async :as a]))
(defn start [c result] (a/go (a/alts! [c (a/timeout 1000)]) (a/close! result)))
(defn drain [c] (a/<!! c))                 ; blocking OUTSIDE go is what <!! is for
(defn worker [c] (a/thread (a/<!! c)))     ; and inside a/thread, an unbounded pool
