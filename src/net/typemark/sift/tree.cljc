(ns net.typemark.sift.tree
  (:require [clojure.string :as str]
            [net.typemark.sift.parse :as parse]))

(defn text
  [nodes n]
  (or (:text n)
      (let [{::parse/keys [source line-starts]} (meta nodes)]
        (when (and source (:line n))
          (let [starts @line-starts]
            (subs source
                  (+ (nth starts (dec (:line n))) (dec (:col n)))
                  (+ (nth starts (dec (:end-line n))) (dec (:end-col n)))))))))

(def literal-types #{:string :number :char :regex})

(defn literal?
  [{:keys [type tag]}]
  (or (contains? literal-types type)
      (contains? #{:quote :syntax-quote} tag)))

(defn- own-index
  [nodes n]
  (let [i (:index n)]
    (when (and i (vector? nodes) (< i (count nodes)) (identical? n (nth nodes i)))
      i)))

(defn children-of
  [nodes n]
  (if-let [i (own-index nodes n)]
    (subvec nodes (inc i) (inc (:last n)))
    (->> nodes
         (filter #(and (or (> (:line %) (:line n))
                           (and (= (:line %) (:line n)) (> (:col %) (:col n))))
                       (or (< (:end-line %) (:end-line n))
                           (and (= (:end-line %) (:end-line n)) (<= (:end-col %) (:end-col n))))))
         distinct)))

(def call-tags
  #{:list :fn})

(defn lists-headed-by
  [nodes heads]
  (filter #(and (contains? call-tags (:tag %))
                (not (:commented? %))
                (not (:quoted? %))
                (contains? heads (:head %)))
          nodes))

(defn arguments
  [nodes lst]
  (let [kids (remove #(= :trivia (:type %)) (children-of nodes lst))
        depth (inc (:depth lst))]
    (->> kids
         (filter #(= depth (:depth %)))
         (remove #(and (= (:line %) (:line lst)) (= (:text %) (:head lst)))))))

(defn parent
  [nodes n]
  (let [d (dec (:depth n))]
    (->> nodes
         (filter #(and (= d (:depth %))
                       (or (< (:line %) (:line n))
                           (and (= (:line %) (:line n)) (<= (:col %) (:col n))))
                       (or (> (:end-line %) (:end-line n))
                           (and (= (:end-line %) (:end-line n)) (>= (:end-col %) (:end-col n))))))
         last)))

(defn let-bound-locals
  [nodes inits]
  (into #{}
        (for [l (lists-headed-by nodes #{"let" "loop" "let*" "when-let" "if-let"})
              v (take 1 (filter #(= :vector (:tag %)) (children-of nodes l)))
              :let [kids (->> (children-of nodes v) (remove #(= :trivia (:type %)))
                              (filter #(= (inc (:depth v)) (:depth %))))]
              [lhs rhs] (partition 2 kids)
              :when (and (= :symbol (:type lhs)) (= :list (:tag rhs)) (contains? inits (:head rhs)))]
          (:text lhs))))

(defn first-argument [nodes lst]
  (first (arguments nodes lst)))

(defn unquote-string
  [{:keys [type text]}]
  (when (= :string type)
    (subs text 1 (max 1 (dec (count text))))))

(defn head-parts
  [head]
  (when head
    (cond
      (str/ends-with? head ".")   [(subs head 0 (dec (count head))) :new]
      (str/includes? head "/")    (let [i (str/last-index-of head "/")]
                                    [(subs head 0 i) (subs head (inc i))])
      :else nil)))
