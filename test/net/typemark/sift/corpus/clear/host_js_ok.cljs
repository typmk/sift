(ns host-js-ok)

(defn node-of [id ns]
  (let [o #js {:id id :ns ns}]
    (str (aget o "ns") "/" (aget o "id"))))

(defn foreign [^js node]
  (.-parent node))
