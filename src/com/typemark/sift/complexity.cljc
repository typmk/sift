(ns com.typemark.sift.complexity
  "Per-function Cognitive Complexity (Campbell, SonarSource 2017) and
  Cyclomatic Complexity (McCabe) over the rewrite-clj tree, plus the two
  shape metrics DCM reports beside them: maximum nesting and parameter count.

  `metrics.cljc` answers per FILE, which is what a Sonar dashboard divides by.
  This answers per UNIT, which is what a reviewer acts on. The two are not
  the same fold: a file total is every increment once, a unit total resets
  nesting at its own boundary and reports the functions inside it as
  children, exactly as cccc-core does — measured against it on this repo so
  that the number a Rust binary would print and the number this prints are
  the same number.

  Two rules where this deliberately departs from cccc, both stated so the
  delta can be measured rather than discovered:

  - `(comment …)` is data. cccc lowers its body as code; clj-kondo, this
    library's `parse` and every reader treat it as inert.
  - `recur` with no enclosing `loop` is recursion, +1, the same as calling
    the function by name. cccc counts only the named call.

  The unit rule is POSITIONAL, not a list of macros: any top-level
  `(def… name …)` whose leaf starts with `def` is a unit named `name`, so
  `deftest`, `deftool`, `defcap` and next week's macro are seen without a
  config nobody fills in. Measured on defnet before that rule existed: 696
  deftests and 31 deftools reported nothing, and the #1 result had no name.

  Sonar's own JS implementation (S3776) scores `||`/`??` at 0 and recursion
  at 0. That is a product decision; this follows the whitepaper, as
  cccc-core does. Expect this to read one higher per `or` sequence and per
  self-call than a Sonar dashboard would."
  (:require [clojure.string :as str]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]))

;; ---- reading the tree ---------------------------------------------------

(def ^:private trivia
  "Whitespace and comments. A `;;` inside a form is a `:comment` NODE in
  rewrite-clj, and a positional read that keeps it takes it for the `then`
  of an `if-let` or the init of a binding — measured on defnet: 26 units
  disagreed with cccc, every one over a comment sitting between forms."
  #{:whitespace :newline :comma :comment :uneval})

(defn- kids
  "Children that are code."
  [node]
  (when (n/inner? node)
    (remove #(contains? trivia (n/tag %)) (n/children node))))

(defn- line-of [node] (:row (meta node)))
(defn- end-line-of [node] (:end-row (meta node)))

(defn- unwrap-meta
  "`^:async x` -> x. Metadata is inert for every rule here."
  [node]
  (if (= :meta (n/tag node))
    (recur (last (kids node)))
    node))

(defn- sym-text
  "The text of a symbol node, or nil."
  [node]
  (let [node (unwrap-meta node)]
    (when (= :token (n/tag node))
      (let [t (n/string node)]
        (when-not (or (str/starts-with? t ":") (str/starts-with? t "\"")
                      (str/starts-with? t "\\") (re-matches #"[-+]?[0-9].*" t)
                      (contains? #{"true" "false" "nil"} t))
          t)))))

(defn- leaf
  "`mcp/deftool` -> `deftool`; a bare `/` is its own leaf."
  [s]
  (if-let [i (and s (str/last-index-of s "/"))]
    (if (< (inc i) (count s)) (subs s (inc i)) s)
    s))

(defn- lst? [node] (contains? #{:list :fn} (n/tag node)))
(defn- vec? [node] (= :vector (n/tag node)))
(defn- head [node] (when (lst? node) (some-> (first (kids node)) sym-text leaf)))

(defn- name-text
  "What a `def…` binds: symbol, keyword or string, seen through `^meta`."
  [node]
  (let [node (unwrap-meta node)]
    (when (= :token (n/tag node))
      (let [t (n/string node)]
        (cond (str/starts-with? t "\"") (subs t 1 (dec (count t)))
              (str/starts-with? t "\\") nil
              (re-matches #"[-+]?[0-9].*" t) nil
              :else t)))))

(defn- fn-form? [node] (contains? #{"fn" "fn*"} (head node)))

;; ---- lowering: tree -> IR -----------------------------------------------
;;
;; The IR is cccc-core's, as maps:
;;   {:ir :function :name :kind :line :end-line :params :body [..]}
;;   {:ir :branch :test [..] :then [..] :else ir-or-nil}   when/cond
;;   {:ir :conditional :test [..] :then [..] :else [..]}   if (a ternary)
;;   {:ir :loop :body [..]}
;;   {:ir :switch :cases [{:default? bool :body [..]} ..]}
;;   {:ir :catch :body [..]}
;;   {:ir :logical :op "and"|"or" :operands [[..] ..]}
;;   {:ir :call :callee "name"}    {:ir :recur}    {:ir :group :body [..]}

(declare lower)

(defn- lower-seq [ctx nodes] (vec (mapcat #(lower ctx %) nodes)))

(defn- string-node?
  "A one-line string is a `:token`; one spanning lines is `:multi-line`.
  Measured: missing the second dropped the body of every defn with a
  multi-line docstring, 350 units on defnet, each scoring 0."
  [node]
  (and node (or (= :multi-line (n/tag node))
                (and (= :token (n/tag node)) (str/starts-with? (n/string node) "\"")))))

(defn- skip-doc-and-meta [nodes]
  (cond-> nodes
    (string-node? (first nodes)) rest
    true (as-> ns' (if (some-> (first ns') n/tag (= :map)) (rest ns') ns'))))

(defn- param-count
  "Symbols in an arg vector, `&` excluded; a destructuring form counts once."
  [argv]
  (count (remove #(= "&" (sym-text %)) (kids argv))))

(defn- arities
  "`[args] body…` or `([args] body…)…` -> [{:params n :body [nodes]} …]."
  [rest-nodes]
  (if (vec? (unwrap-meta (or (first rest-nodes) (n/token-node nil))))
    [{:params (param-count (unwrap-meta (first rest-nodes))) :body (rest rest-nodes)}]
    (for [clause rest-nodes
          :when (lst? clause)
          :let [[argv & body] (kids clause)]
          :when (and argv (vec? (unwrap-meta argv)))]
      {:params (param-count (unwrap-meta argv)) :body body})))

(defn- function-ir [ctx node name kind rest-nodes]
  (let [as (arities rest-nodes)]
    {:ir :function :name name :kind kind
     :line (line-of node) :end-line (end-line-of node)
     :params (reduce max 0 (map :params as))
     :body (lower-seq (assoc ctx :top? false) (mapcat :body as))}))

(defn- fn-name-and-rest [items]
  (if-let [nm (some-> (second items) sym-text)]
    [nm (drop 2 items)]
    ["<fn>" (rest items)]))

(defn- inline-fn
  "A `(fn …)` directly in a def body, or one call-wrapper deep
  (`(with-auto-index (fn …))`), is the unit's own body. Returns IR nodes, or
  nil when `node` is neither."
  [ctx node wrap]
  (when (lst? node)
    (let [items (kids node)]
      (cond
        (fn-form? node)
        (let [[_ rest-nodes] (fn-name-and-rest items)]
          (lower-seq (assoc ctx :top? false) (mapcat :body (arities rest-nodes))))

        (and (pos? wrap) (sym-text (first items)) (some fn-form? (rest items)))
        (into [{:ir :call :callee (sym-text (first items))}]
              (mapcat #(or (inline-fn ctx % (dec wrap)) (lower ctx %)) (rest items)))))))

(defn- def-macro-ir
  "`(def<x> name doc? meta? body…)`: a unit named `name`, kind = the macro."
  [ctx node items kind]
  (let [nm   (or (name-text (second items)) "<def>")
        body (skip-doc-and-meta (drop 2 items))]
    {:ir :function :name nm
     :kind (case kind ("def" "defonce") "def" kind)
     :line (line-of node) :end-line (end-line-of node)
     :params 0
     :body (let [ctx (assoc ctx :top? false)]
             (vec (mapcat #(or (inline-fn ctx % 1) (lower ctx %)) body)))}))

(defn- method-ir? [node]
  (and (lst? node)
       (let [[m argv] (kids node)]
         (and m (sym-text m) argv (vec? (unwrap-meta argv))))))

(defn- methods-ir
  "`defrecord`/`deftype` (`fixed?` — the type name is the owner) and
  `reify`/`extend-protocol`/…: every `(m [args] body…)` is a `method` unit
  named `Owner/m`. Under `extend-*` a bare symbol renames the owner."
  [ctx node items fixed?]
  (let [h     (head node)
        owner (if fixed? (or (some-> (second items) sym-text) "<type>") h)
        start (if fixed? 3 1)]
    (loop [xs (drop start items) owner owner acc []]
      (if-let [x (first xs)]
        (cond
          (and (not fixed?) (contains? #{"extend-protocol" "extend-type" "extend"} h)
               (sym-text x))
          (recur (rest xs) (leaf (sym-text x)) acc)

          (method-ir? x)
          (let [[m argv & body] (kids x)]
            (recur (rest xs) owner
                   (conj acc {:ir :function :kind "method"
                              :name (str owner "/" (sym-text m))
                              :line (line-of x) :end-line (end-line-of x)
                              :params (param-count (unwrap-meta argv))
                              :body (lower-seq (assoc ctx :top? false) body)})))

          :else (recur (rest xs) owner (into acc (lower (assoc ctx :top? false) x))))
        acc))))

(defn- binding-inits
  "A binding vector is flat `[name init …]`; the inits are code."
  [ctx bvec]
  (when (and bvec (vec? bvec))
    (lower-seq ctx (take-nth 2 (rest (kids bvec))))))

(defn- else-keyword? [node]
  (contains? #{":else" ":default"} (some-> node n/string)))

(defn- cond-chain
  "`cond` pairs -> a branch chain: the first test is structural, each next
  one is an `else if` (+1 flat), `:else` is a plain else."
  [ctx pairs]
  (when-let [[test expr & more] (seq pairs)]
    (if (else-keyword? test)
      {:ir :group :body (lower-seq ctx (if expr [expr] []))}
      {:ir :branch :test (lower ctx test) :then (lower-seq ctx (if expr [expr] []))
       :else (cond-chain ctx more)})))

(defn- switch-ir [ctx items condp?]
  (let [pre     (if condp? (take 2 (rest items)) (take 1 (rest items)))
        clauses (drop (if condp? 3 2) items)]
    (into (lower-seq ctx pre)
          [{:ir :switch
            :cases (loop [cs clauses acc []]
                     (cond (empty? cs) acc
                           (= 1 (count cs)) (conj acc {:default? true :body (lower ctx (first cs))})
                           :else (recur (drop 2 cs)
                                        (conj acc {:default? false :body (lower ctx (second cs))}))))}])))

(defn- try-ir [ctx items]
  (vec (mapcat (fn [x]
                 (case (head x)
                   "catch"   [{:ir :catch :body (lower-seq ctx (drop 3 (kids x)))}]
                   "finally" (lower-seq ctx (rest (kids x)))
                   (lower ctx x)))
               (rest items))))

(defn- logical-ir [ctx op args]
  ;; Like operators flatten into one sequence; a different operator nested
  ;; inside is its own sequence, which is what visit_logical charges for.
  ;; Only a `(or …)` LIST flattens; `#(or …)` is a lambda whose body
  ;; happens to start with `or`, and it is its own unit.
  (let [operands (mapcat (fn [a]
                           (if (and (= :list (n/tag a)) (= op (head a)))
                             (:operands (first (logical-ir ctx op (rest (kids a)))))
                             [(lower ctx a)]))
                         args)]
    (if (>= (count operands) 2)
      [{:ir :logical :op op :operands (vec operands)}]
      (vec (apply concat operands)))))

(def ^:private transparent
  #{"let" "let*" "when-let*" "if-let*" "binding" "with-open" "with-local-vars"
    "with-redefs" "with-bindings" "dosync"})

(def ^:private sequencing
  #{"do" "doto" "->" "->>" "as->" "some->" "some->>" "delay" "future" "locking"
    "time" "doall" "dorun" "vary-meta"})

(def ^:private declarations
  #{"ns" "defprotocol" "definterface" "declare" "import" "require" "use"
    "gen-class" "quote" "comment"})

(defn- lower-list [ctx node]
  (let [items (kids node)
        h     (head node)]
    (if (empty? items)
      []
    (case h
      ("defn" "defn-" "defmacro")
      [(function-ir ctx node (or (name-text (second items)) "<defn>") "defn"
                    (skip-doc-and-meta (drop 2 items)))]

      ("fn" "fn*")
      (let [[nm rest-nodes] (fn-name-and-rest items)]
        [(function-ir ctx node nm "fn" rest-nodes)])

      "defmethod"
      (let [i (first (keep-indexed (fn [i x] (when (and (> i 1) (vec? (unwrap-meta x))) i)) items))]
        [(function-ir ctx node (or (name-text (second items)) "<defmethod>") "defmethod"
                      (if i (drop i items) []))])

      "letfn"
      (into (vec (for [b (kids (second items))
                       :when (and (lst? b) (sym-text (first (kids b))))]
                   (function-ir ctx b (sym-text (first (kids b))) "letfn" (rest (kids b)))))
            (lower-seq ctx (drop 2 items)))

      ("defrecord" "deftype") (methods-ir ctx node items true)
      ("reify" "extend-protocol" "extend-type" "specify!" "proxy" "extend")
      (methods-ir ctx node items false)

      ;; `if` is an expression, so it is scored as Sonar scores a ternary:
      ;; +1 and nesting, both arms nested, and no `else` increment — a
      ;; Clojure `if` has no else keyword to charge for. `cond` chains do
      ;; charge for `:else`, as a statement chain would.
      ("if" "if-not" "if-let" "if-some")
      (let [[_ test then else] items]
        [{:ir :conditional
          :test (lower ctx test)
          :then (lower-seq ctx (if then [then] []))
          :else (lower-seq ctx (if else [else] []))}])

      ("when" "when-not" "when-let" "when-some" "when-first")
      [{:ir :branch :test (lower ctx (second items))
        :then (lower-seq ctx (drop 2 items)) :else nil}]

      "cond" (if-let [c (cond-chain ctx (rest items))] [c] [])

      ("cond->" "cond->>")
      (into (lower ctx (second items))
            (for [[test form] (partition-all 2 (drop 2 items))]
              {:ir :branch :test (lower ctx test)
               :then (lower-seq ctx (if form [form] [])) :else nil}))

      "case"  (switch-ir ctx items false)
      "condp" (switch-ir ctx items true)

      ("and" "or") (logical-ir ctx h (rest items))

      ;; Binding inits are evaluated OUTSIDE the loop's nesting — a `:when`
      ;; test in a `for` is not inside the body it guards.
      ("loop" "doseq" "dotimes" "for")
      (conj (binding-inits ctx (second items))
            {:ir :loop :body (lower-seq (assoc ctx :in-loop? true) (drop 2 items))})
      "while" [{:ir :loop :body (lower-seq (assoc ctx :in-loop? true) (rest items))}]

      "try" (try-ir ctx items)

      "recur" (if (:in-loop? ctx)
                (lower-seq ctx (rest items))
                (into [{:ir :recur}] (lower-seq ctx (rest items))))

      (cond
        (contains? transparent h)
        (into (binding-inits ctx (second items)) (lower-seq ctx (drop 2 items)))

        (contains? sequencing h) (lower-seq ctx (rest items))
        (contains? declarations h) []

        (and (:top? ctx) h (str/starts-with? h "def") (name-text (second items)))
        [(def-macro-ir ctx node items h)]

        :else
        (into [{:ir :call :callee (some-> (first items) sym-text)}]
              (lower-seq ctx (if (sym-text (first items)) (rest items) items))))))))

(defn lower
  "One rewrite-clj node -> IR nodes."
  [ctx node]
  (case (n/tag node)
    :list (lower-list ctx node)
    (:vector :map :set) (lower-seq ctx (kids node))
    ;; `#(…)` is the list itself with a `#` in front: its head is its first
    ;; child, so the body is the form lowered as a list.
    :fn [{:ir :function :name "<fn>" :kind "fn"
          :line (line-of node) :end-line (end-line-of node)
          :params (count (distinct (re-seq #"%\d*&?" (n/string node))))
          :body (lower-list (assoc ctx :top? false) node)}]
    :meta (lower ctx (last (kids node)))
    (:quote :syntax-quote :uneval :regex :token :comment) []
    (:deref :var :unquote :unquote-splicing) (lower-seq ctx (kids node))
    :reader-macro
    (let [[tag payload] (kids node)
          t (some-> tag n/string)]
      (cond
        (= t "js") (lower ctx payload)
        (= t "?")  (let [pairs (partition-all 2 (kids payload))
                         want  (:features ctx)]
                     (or (some (fn [[k v]]
                                 (when (contains? want (some-> k n/string))
                                   (lower ctx v)))
                               pairs)
                         []))
        :else []))
    (if (n/inner? node) (lower-seq ctx (kids node)) [])))

;; ---- scoring: IR -> report ----------------------------------------------

(declare score-node score-function)
(defn- score-nodes [frame nodes n]
  (reduce #(score-node %1 %2 n) frame nodes))

(defn- structural [frame n]
  (-> frame (update :cognitive + 1 n) (update :cyclomatic inc)
      (update :max-nesting max (inc n))))

(defn- score-else [frame else n]
  (cond
    (nil? else) frame
    (= :branch (:ir else))
    (-> frame (update :cognitive inc) (update :cyclomatic inc)
        (score-nodes (:test else) n)
        (score-nodes (:then else) (inc n))
        (score-else (:else else) n))
    :else (-> frame (update :cognitive inc) (score-nodes (:body else) (inc n)))))


(defn score-node [frame node n]
  (case (:ir node)
    :function (update frame :children conj (score-function node))
    :branch   (-> (structural frame n)
                  (score-nodes (:test node) n)
                  (score-nodes (:then node) (inc n))
                  (score-else (:else node) n))
    :conditional (-> (structural frame n)
                     (score-nodes (:test node) n)
                     (score-nodes (:then node) (inc n))
                     (score-nodes (:else node) (inc n)))
    :loop     (-> (structural frame n) (score-nodes (:body node) (inc n)))
    :catch    (-> (structural frame n) (score-nodes (:body node) (inc n)))
    :switch   (reduce (fn [f {:keys [default? body]}]
                        (-> (cond-> f (not default?) (update :cyclomatic inc))
                            (score-nodes body (inc n))))
                      (-> frame (update :cognitive + 1 n) (update :max-nesting max (inc n)))
                      (:cases node))
    :logical  (-> frame (update :cognitive inc)
                  (update :cyclomatic + (dec (count (:operands node))))
                  (score-nodes (apply concat (:operands node)) n))
    :call     (cond-> frame (= (:callee node) (:name frame)) (update :cognitive inc))
    :recur    (update frame :cognitive inc)
    :group    (score-nodes frame (:body node) n)
    frame))

(defn- score-function [{:keys [name kind line end-line params body]}]
  (let [f (score-nodes {:name name :kind kind :line line :end-line end-line
                        :params params :cognitive 0 :cyclomatic 1 :max-nesting 0
                        :children []}
                       body 0)]
    (assoc f :loc (inc (- end-line line)))))

;; ---- entry points -------------------------------------------------------

(defn- features-for [path]
  (if (and path (str/ends-with? path ".cljs")) #{":cljs" ":default"} #{":clj" ":default"}))

(defn- functions-in
  "Every `:function` IR node under `nodes`, at any depth, in order — a
  lambda inside module-level code (`(when x (.on p \"exit\" #(…)))`) is a
  unit even though nothing named encloses it."
  [nodes]
  (mapcat (fn [node]
            (if (= :function (:ir node))
              [node]
              (functions-in (concat (:test node) (:then node) (:body node)
                                    (when (map? (:else node)) [(:else node)])
                                    (when (sequential? (:else node)) (:else node))
                                    (apply concat (:operands node))
                                    (mapcat :body (:cases node))))))
          nodes))

(defn report
  "Source -> {:ok? true :functions [unit …]} or {:ok? false :error msg}.

  A unit is {:name :kind :line :end-line :loc :params :cognitive :cyclomatic
  :max-nesting :children [unit …]}. `path` decides which `#?` branch is
  scored (`.cljs` -> :cljs, else :clj); nil means :clj."
  ([source] (report source nil))
  ([source path]
   (try
     (let [ctx   {:top? true :features (features-for path)}
           units (lower-seq ctx (kids (p/parse-string-all source)))]
       {:ok? true
        :functions (mapv score-function (functions-in units))})
     (catch #?(:clj Exception :cljs :default) e
       {:ok? false :error #?(:clj (.getMessage e) :cljs (ex-message e))}))))

(defn flatten-units
  "Every unit at every depth, parents before children."
  [units]
  (mapcat (fn [u] (cons (dissoc u :children) (flatten-units (:children u)))) units))

(def default-threshold
  "Sonar's default for S3776. A function above it is reported."
  15)

(defn findings
  "Units over `threshold`, in the shape every other sift rule emits."
  ([source path] (findings source path default-threshold))
  ([source path threshold]
   (let [{:keys [ok? functions]} (report source path)]
     (when ok?
       (for [u (flatten-units functions) :when (> (:cognitive u) threshold)]
         {:rule    :cognitive-complexity
          :evidence :parity :note "5,519/5,531 units vs cccc on defnet; Spearman 0.989 vs SonarJS on LightTable"
          ;; the unit's name as :symbol, not only in the message — the self-run
          ;; found every complexity finding carrying :symbol nil
          :symbol  (some-> (:name u) symbol)
          :line    (:line u) :col 1 :end-line (:line u) :end-col 2
          :message (str (:name u) " has cognitive complexity " (:cognitive u)
                        " (threshold " threshold "); cyclomatic " (:cyclomatic u)
                        ", nesting " (:max-nesting u))
          :cognitive (:cognitive u)
          :function (:name u)})))))
