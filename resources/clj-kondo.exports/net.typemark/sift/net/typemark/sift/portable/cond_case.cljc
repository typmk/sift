(ns net.typemark.sift.portable.cond-case)

(defn- literal? [v]
  (or (keyword? v) (number? v) (string? v) (char? v) (nil? v) (boolean? v)))

(defn- case-key?
  [v]
  (or (keyword? v) (number? v) (string? v) (char? v)))

(defn- eq-test
  [form]
  (when (and (seq? form) (= '= (first form)) (= 3 (count form)))
    (let [[_ a b] form]
      (cond (and (symbol? a) (literal? b)) [a b]
            (and (symbol? b) (literal? a)) [b a]))))

(defn check
  [form]
  (let [xs (rest form)]
    (when (and (seq? form) (even? (count xs)))
      (let [cs (partition 2 xs)
            else? (contains? #{:else :default} (first (last cs)))
            body (if else? (butlast cs) cs)
            tests (map (comp eq-test first) body)]
        (when (and (>= (count body) 2)
                   (every? some? tests)
                   (apply = (map first tests)))
          (let [x (ffirst tests)
                ks (map second tests)
                mechanical? (every? case-key? ks)]
            (cond-> {:symbol x
                     :message (str "cond compares " x " against " (count body) " literals; use case")
                     :applicability (if mechanical? :machine-applicable :unspecified)}
              mechanical?
              (assoc :fix
                     (concat (list 'case x)
                             (mapcat (fn [[k [_ expr]]] [k expr]) (map vector ks body))
                             [(if else? (second (last cs)) nil)])))))))))
