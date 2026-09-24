(ns net.typemark.sift.comments
  (:require [clojure.string :as str]))

(defn- within? [outer n]
  (and (not= outer n)
       (or (< (:line outer) (:line n)) (and (= (:line outer) (:line n)) (<= (:col outer) (:col n))))
       (or (> (:end-line outer) (:end-line n)) (and (= (:end-line outer) (:end-line n)) (>= (:end-col outer) (:end-col n))))))

(defn- outermost [ns]
  (remove (fn [n] (some #(within? % n) ns)) ns))

(defn- line-comment? [n]
  (and (= :comment (:type n))
       (not (:commented? n))
       (not (and (= 1 (:line n)) (str/starts-with? (:text n) "#!")))))

(defn- blocks [ns]
  (reduce (fn [acc n]
            (let [prev (peek acc)]
              (if (and prev (not (:trailing? prev)) (not (:trailing? n)) (= (:line n) (inc (:end-line prev))))
                (conj (pop acc) (-> prev (assoc :end-line (:line n) :end-col (:end-col n)) (update :texts conj (:text n))))
                (conj acc (assoc n :texts [(:text n)])))))
          []
          ns))

(defn- span [n]
  {:line (:line n) :column (:col n) :end-line (:end-line n) :end-column (:end-col n)})

(defn- end-of-text [n]
  (let [t (str/trimr (:text n))]
    (assoc n :end-line (:line n) :end-col (+ (:col n) (count t)))))

(defn findings
  [nodes {:keys [allow kinds]}]
  (let [kinds (set (or kinds [:line :discard :rich]))
        code-ends (reduce (fn [m n] (update m (:end-line n) (fnil min (:end-col n)) (:end-col n)))
                          {}
                          (remove #(or (:inner? %) (#{:comment :trivia} (:type %))) nodes))
        trailing (fn [n] (assoc n :trailing? (boolean (some-> (code-ends (:line n)) (<= (:col n))))))
        allowed? (if allow (let [re (re-pattern allow)] (fn [b] (some #(re-find re %) (:texts b)))) (constantly false))
        lines (->> nodes (filter line-comment?) (map end-of-text) (map trailing) (sort-by (juxt :line :col)) blocks (remove allowed?))
        discards (outermost (filter #(= :uneval (:tag %)) nodes))
        rich (outermost (filter #(and (= :list (:tag %)) (= "comment" (:head %))) nodes))]
    (concat
     (when (kinds :line)
       (for [b lines]
         (assoc (span b) :kind :line
                :message (if (= 1 (count (:texts b))) "comment" (str (count (:texts b)) "-line comment")))))
     (when (kinds :discard)
       (for [n (remove (fn [n] (some #(within? % n) rich)) discards)]
         (assoc (span n) :kind :discard :message "code discarded with #_")))
     (when (kinds :rich)
       (for [n (remove (fn [n] (some #(within? % n) discards)) rich)]
         (assoc (span n) :kind :rich :message "(comment …) block"))))))
