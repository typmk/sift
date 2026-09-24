(ns corpus.flag.swap-in-anon-fn)
;; #(…) is ONE :fn node with no list node for its body, so this deref of the
;; atom its own swap! is computing was invisible to :inside until 2026-09-10.
(def cache (atom {}))
(defn remember [k v]
  (map #(swap! cache assoc % (merge (get @cache %) v)) [k]))
