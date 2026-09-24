(ns corpus.flag.catalogue-case-else)
(defn orient [result]
  (case result "portrait" :p "landscape" :l :else :default))
