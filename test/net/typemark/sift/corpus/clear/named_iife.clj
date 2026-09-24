(ns named-iife
  "HARD NEGATIVE. Named IIFE walk, modelled on a production accumulator.")

(defn locals [form]
  (let [acc (atom [])]
    ((fn walk [f]
       (when (symbol? f)
         (swap! acc conj f))
       (when (coll? f)
         (doseq [x f] (walk x))))
     form)
    @acc))
