(ns generated
  "Control B. The shape an agent produces. Must produce findings."
  (:require [clojure.string :as str]))

(defn summarise [requests]
  (let [result (atom {})]
    (if (nil? requests)
      {}
      (do
        (doseq [r requests]
          (if (not (nil? r))
            (let [supplier (get r :supplier)
                  product  (get r :product)
                  qty      (get r :qty)
                  reason   (get r :reason)]
              (if (not (nil? supplier))
                (let [line {:product product
                            :qty     (if (nil? qty) 0 qty)
                            :reason  (if (nil? reason) "unspecified" reason)
                            :supplier supplier}]
                  (if (contains? @result supplier)
                    (swap! result assoc supplier (conj (get @result supplier) line))
                    (swap! result assoc supplier [line])))
                nil))
            nil))
        @result))))

(defn describe [summary]
  (let [out (atom [])]
    (if (nil? summary)
      ""
      (do
        (doseq [entry (sort-by (fn [e] (- (count (val e)))) summary)]
          (let [supplier (key entry)
                lines    (val entry)
                total    (atom 0)]
            (doseq [l lines]
              (swap! total + (if (nil? (get l :qty)) 0 (get l :qty))))
            (swap! out conj (str supplier ": " (count lines) " lines, " @total " units"))))
        (str/join "\n" @out)))))
