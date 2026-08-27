(ns returned-fn
  "HARD NEGATIVE. The atom is a cell closed over by a returned fn.")

(defn counter []
  (let [n (atom 0)]
    (fn [] (swap! n inc))))
