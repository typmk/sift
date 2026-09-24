(ns host-js-prop)

(defn node-of [id ns]
  (let [o #js {:id id :ns ns}]
    (str (.-ns o) "/" (.-id o))))
