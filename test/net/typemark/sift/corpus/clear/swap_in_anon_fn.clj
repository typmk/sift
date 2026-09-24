(ns corpus.clear.swap-in-anon-fn)
;; the same shape with a DIFFERENT atom read: :inside carries ?a, so this is
;; not a lost update and must stay silent now that #() is walked into.
(def cache (atom {}))
(def defaults (atom {}))
(defn remember [k v]
  (map #(swap! cache assoc % (merge (get @defaults %) v)) [k]))
;; and the anon fn with no deref at all
(defn bump [k] (map #(swap! cache update % (fnil inc 0)) [k]))
