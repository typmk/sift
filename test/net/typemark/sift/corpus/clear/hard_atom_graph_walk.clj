(ns hard-atom-graph-walk
  "HARD NEGATIVE. Mutation lives in a named recursive walk (letfn).")

(defn transitive-needs [graph root]
  (let [visited (atom #{}) refs (atom [])]
    (letfn [(walk [n]
              (when-not (@visited n)
                (swap! visited conj n)
                (swap! refs conj n)
                (run! walk (get graph n))))]
      (walk root))
    {:visited @visited :refs @refs}))
