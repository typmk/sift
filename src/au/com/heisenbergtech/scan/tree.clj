(ns au.com.heisenbergtech.scan.tree
  "Queries over the flat node stream that more than one rule set needs.

  Extracted when the Java-interop rules arrived and would otherwise have
  become a third copy of `children-of` and friends."
  (:require [clojure.string :as str]))

(def literal-types #{:string :number :char :regex})

(defn literal?
  "A value that cannot carry an attacker's payload. A quoted form counts:
  `(eval '(inc 1))` is the commonest safe use of eval, and flagging it would
  train people to ignore the rule."
  [{:keys [type tag]}]
  (or (contains? literal-types type)
      (contains? #{:quote :syntax-quote} tag)))

(defn children-of
  "Nodes strictly inside `n`. The flat stream keeps document order, so a
  node's subtree is what starts after it and ends within it."
  [nodes n]
  (->> nodes
       (filter #(and (or (> (:line %) (:line n))
                         (and (= (:line %) (:line n)) (> (:col %) (:col n))))
                     (<= (:end-line %) (:end-line n))))
       distinct))

(def call-tags
  "Forms that invoke: a list, and an anonymous-fn literal."
  #{:list :fn})

(defn lists-headed-by
  "Live, unquoted call forms whose head symbol is in `heads`.

  Quoted forms are excluded: nothing inside `'[...]` is invoked, so a datalog
  clause is not a function call."
  [nodes heads]
  (filter #(and (contains? call-tags (:tag %))
                (not (:commented? %))
                (not (:quoted? %))
                (contains? heads (:head %)))
          nodes))

(defn arguments
  "The argument nodes of a call, in order, excluding trivia and the head."
  [nodes lst]
  (let [kids (remove #(= :trivia (:type %)) (children-of nodes lst))
        depth (inc (:depth lst))]
    (->> kids
         (filter #(= depth (:depth %)))
         (remove #(and (= (:line %) (:line lst)) (= (:text %) (:head lst)))))))

(defn first-argument [nodes lst]
  (first (arguments nodes lst)))

(defn unquote-string
  "The content of a string literal node, or nil when it is not one."
  [{:keys [type text]}]
  (when (= :string type)
    (subs text 1 (max 1 (dec (count text))))))

(defn head-parts
  "A call head like `MessageDigest/getInstance` or `Random.` split into
  [class-name member], where member is `:new` for a constructor form."
  [head]
  (when head
    (cond
      (str/ends-with? head ".")   [(subs head 0 (dec (count head))) :new]
      (str/includes? head "/")    (let [i (str/last-index-of head "/")]
                                    [(subs head 0 i) (subs head (inc i))])
      :else nil)))
