(ns named-fn-inner-anon
  "HARD NEGATIVE. Named walk; mutation is inside an inner anonymous fn.
   Modelled on a production walker.")

(defn lift [forms]
  (let [!ops (atom [])
        rewrite
        (fn rewrite [form]
          (if (map? form)
            (reduce-kv
             (fn [m k v]
               (swap! !ops conj k)
               (assoc m k (rewrite v)))
             {} form)
            form))]
    (rewrite forms)
    @!ops))
