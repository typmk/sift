(ns net.typemark.sift.tree
  "Queries over the flat node stream that more than one rule set needs.

  Extracted when the Java-interop rules arrived and would otherwise have
  become a third copy of `children-of` and friends."
  (:require [clojure.string :as str]
            [net.typemark.sift.parse :as parse]))

(defn text
  "The source text of `n`, a node of `nodes`. A leaf carries it as :text; an
  inner node's is sliced from the source `parse` keeps on the stream's
  metadata. nil for an inner node of a stream `parse` did not build."
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
  "A value that cannot carry an attacker's payload. A quoted form counts:
  `(eval '(inc 1))` is the commonest safe use of eval, and flagging it would
  train people to ignore the rule."
  [{:keys [type tag]}]
  (or (contains? literal-types type)
      (contains? #{:quote :syntax-quote} tag)))

(defn- own-index
  "`n`'s :index when `nodes` is the vector `parse` built and `n` sits at that
  index in it, else nil."
  [nodes n]
  (let [i (:index n)]
    (when (and i (vector? nodes) (< i (count nodes)) (identical? n (nth nodes i)))
      i)))

(defn children-of
  "Nodes strictly inside `n`. The stream is pre-order, so a node's subtree is
  the run `parse` recorded from :index to :last, and this slices it. A
  stream that is not `parse`'s own vector is scanned by span, end column
  included."
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

(defn parent
  "The node immediately enclosing `n` — depth one less, span containing —
  or nil at top level. A string that is a MAP KEY has a :map parent and a
  string that is a CALL ARGUMENT has a :list one; the permissions rule
  could not tell \"401\" the HTTP status from \"401\" the mode until it
  could ask."
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
  "Names bound in any let/loop/binding vector in the stream to a call whose
  head is in `inits` — `(let [decision (atom nil)] …)` -> #{\"decision\"}."
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
