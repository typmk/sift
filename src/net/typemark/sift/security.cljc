(ns net.typemark.sift.security
  "Security rules that have no call to hang a clj-kondo hook on.

  The call-shaped rules -- eval, read-string, sh, jdbc, MessageDigest and the
  rest -- moved to clj-kondo hooks, where they key on the RESOLVED
  var instead of matching a name as text. What remains here is shape-based: a
  def whose NAME looks like a credential, a permission mode written as a
  string literal, a shell call with only literal arguments.

  The sink and source sets survive for one reason: seeding the interprocedural
  pass in net.typemark.sift.callgraph, which needs to know which vars reach a sink even
  though the direct finding is now raised by a hook."
  (:require [net.typemark.sift.parse :as parse]
            [net.typemark.sift.tree :as tree]))

(def ^:private sources
  "Forms whose result is attacker-influenced. Deliberately small: a false
  source produces a false flow, and a security rule nobody trusts is worse
  than no rule."
  #{"slurp" "read-line" "System/getenv" "System/getProperty"
    ".getParameter" ".getHeader" ".getQueryString" ".getInputStream"
    ":params" ":query-params" ":form-params" ":body" ":json-params" ":headers"
    ":query-string" ":path-params" ":multipart-params"})

(def ^:private eval-fns        #{"eval" "load-string" "load-reader"})
(def ^:private read-string-fns #{"read-string" "clojure.core/read-string"})
(def ^:private shell-fns       #{"sh" "clojure.java.shell/sh" ".exec" "shell"})
(def ^:private sql-fns         #{"query" "execute!" "jdbc/query" "jdbc/execute!"
                                 "sql/query" "db/query" "execute-one!"})
(def ^:private build-fns       #{"str" "format" "clojure.core/str" "clojure.core/format"})
(def ^:private xml-fns         #{"clojure.data.xml/parse" "xml/parse" "parse-str" "xml/parse-str"})

(def ^:private credential-name
  "Anchored on segment boundaries. Unanchored, `bypass` matches `pass` and
  `tokenizer` matches `token` -- and a security rule that cries wolf is a
  security rule nobody reads."
  #"(?i)(^|[-_*/.])(passwords?|passwds?|secrets?|api[-_]?keys?|tokens?|credentials?|private[-_]?keys?|access[-_]?keys?|client[-_]?secrets?)([-_*?!]|$)")

(defn- finding [rule node message]
  {:rule rule
   :line (:line node) :col (:col node)
   :end-line (:end-line node) :end-col (:end-col node)
   :message message})

(defn- dynamic-arg? [nodes lst]
  (let [a (tree/first-argument nodes lst)]
    (and a (not (tree/literal? a)))))

(defn- any-dynamic-arg?
  "True when ANY argument is computed. A shell call is dangerous because of
  the argument that carries the payload, which is rarely the first one."
  [nodes lst]
  (->> (tree/children-of nodes lst)
       (remove #(= :trivia (:type %)))
       (remove #(= (:head lst) (:text %)))
       (filter #(contains? #{:symbol :list} (if (= :list (:tag %)) :list (:type %))))
       (some #(not (tree/literal? %)))
       boolean))

(defn- world-accessible?
  "The others digit carries a bit. The previous pattern required a LEADING 7,
  so \"0700\" (owner-only) fired and \"0666\" did not."
  [s]
  (boolean
   (and s
        (or (and (re-matches #"0?[0-7]{3}" s)
                 (pos? (bit-and (parse-long (str (last s))) 2r011)))
            (re-matches #"[-dlbcps][-r][-w][-xsS][-r][-w][-xsS][-r][-w][-xtT]" s)
            (re-matches #"[-r][-w][-xsS][-r][-w][-xsS][-r][-w][-xtT]" s)))))

(defn findings
  "Shape-based security findings for one parsed file."
  [nodes]
  (concat
   (for [l (tree/lists-headed-by nodes shell-fns)
         :when (not (any-dynamic-arg? nodes l))]
     (finding "shell-invocation" l
              "shell invocation -- confirm no argument is caller-controlled"))

   ;; The def's VALUE, not its docstring.
   (for [l (tree/lists-headed-by nodes #{"def" "defonce"})
         :let [args (tree/arguments nodes l)
               nm   (first args)
               nm-text (some->> nm (tree/text nodes))
               v    (let [lst (last args)]
                      (when (and (= :string (:type lst))
                                 (or (= 2 (count args))
                                     (not= lst (second args))))
                        lst))]
         :when (and nm v (re-find credential-name nm-text)
                    (> (count (:text v)) 6)
                    ;; `ceremony-secret-type` names a KIND of secret, not one:
                    ;; four of four hits on a production service, every value a slug. The name
                    ;; decides, not the value — "sk-live-abcdef" is kebab too.
                    (not (re-find #"(?i)[-_](type|kind|name|id|header|field|param|path|env|key-name)$" nm-text)))]
     (finding "hardcoded-credential" v
              (str "credential-shaped name '" nm-text "' is bound to a literal")))

   (for [l (tree/lists-headed-by nodes xml-fns)]
     (finding "xml-external-entity" l
              "confirm this parser has external entity resolution disabled"))

   ;; Only as the ARGUMENT of a call that sets a mode. \"401\" and \"403\" as
   ;; map keys in an OpenAPI response table read as modes 401 and 403 —
   ;; four of four hits on a production service — and a status code is not a permission.
   (for [n nodes
         :when (and (= :string (:type n)) (not (:commented? n)))
         :let [s (tree/unquote-string n)]
         :when (world-accessible? s)
         :let [p (tree/parent nodes n)]
         :when (and p (contains? tree/call-tags (:tag p))
                    (re-find #"(?i)chmod|perm|mode|umask|^sh$|shell|exec|mkdir|create|open|write" (or (:head p) "")))]
     (finding "permissive-file-permissions" n
              (str "mode " s " grants access to others")))))

(defn- namespace-name [nodes]
  (when-let [nsform (first (tree/lists-headed-by nodes #{"ns"}))]
    (some->> (tree/first-argument nodes nsform) (tree/text nodes))))

(defn- enclosing-var
  "The name of the innermost defn/def enclosing `n`, by position."
  [nodes n]
  (->> (tree/lists-headed-by nodes #{"defn" "defn-" "def" "defmacro" "defmethod"})
       (filter #(and (<= (:line %) (:line n)) (>= (:end-line %) (:line n))))
       (sort-by #(- (:end-line %) (:line %)))
       first
       (#(when % (tree/first-argument nodes %)))
       (#(when % (tree/text nodes %)))))

(defn- dangerous-sinks
  "Sink calls that are actually unsafe, not merely sinks.

  Seeding from raw function NAMES was wrong: it flagged a correctly
  parameterised `jdbc/execute!` because the name matched, and produced six
  false interprocedural paths in a production service."
  [nodes]
  (concat
   (filter #(dynamic-arg? nodes %) (tree/lists-headed-by nodes eval-fns))
   (tree/lists-headed-by nodes read-string-fns)
   (filter #(any-dynamic-arg? nodes %) (tree/lists-headed-by nodes shell-fns))
   (filter (fn [l] (some #(and (= :list (:tag %)) (contains? build-fns (:head %)))
                         (tree/children-of nodes l)))
           (tree/lists-headed-by nodes sql-fns))))

;; Datalog is deliberately NOT seeded here. The SQL heuristic -- a `str` inside
;; the call means the statement is being assembled -- does not transfer: a
;; Datomic transaction is a data structure, and computed values inside it are
;; normal. Measured: seeding it flagged `revoke-jti!`, whose only sin is
;; `(str jti)` to coerce a token id, and fifteen others like it.
;;
;; The risky Datalog shape is that the QUERY ITSELF is computed, which needs
;; the argument position to state. `clj-datalog-query-built` in
;; opengrep/clojure-taint.yml expresses it exactly, with focus-metavariable on
;; the query position; this pass has no argument positions and so cannot.

(defn- get-in-sources
  "`(get-in req [:params :n])` is how request data is most often read, and it
  never matched: `sources` are matched as CALL HEADS, and here the source
  keyword sits inside a path vector. Measured -- across three services
  there are 8 of these and 35 of the `(:params req)` form the head match does
  catch, so a third of the real sources were invisible, and with them every
  interprocedural path that started at one."
  [nodes]
  (for [l (tree/lists-headed-by nodes #{"get-in" "get"})
        :when (some #(and (= :keyword (:type %)) (contains? sources (:text %)))
                    (tree/children-of nodes l))]
    l))

(defn- statement-arg
  "The statement: the first element of the `[sql & params]` vector. Reading
  the wrong position is how a correctly parameterised call gets flagged."
  [nodes l]
  (when-let [v (some (fn [x] (when (= :vector (:tag x)) x)) (tree/children-of nodes l))]
    (some (fn [x] (when (= (inc (:depth v)) (:depth x)) x)) (tree/children-of nodes v))))

(defn- reaching-sinks
  "SQL calls whose statement is a bare symbol -- a string that arrived from
  somewhere else.

  Narrower than it first was, and the narrowing is measured. Seeding on any
  non-literal statement, plus Datalog, produced 57 interprocedural findings
  across three services; the two sampled by hand were a route builder and
  an audit-log helper recording a client IP. In a Datomic codebase a query is
  DATA, and building one from a map is idiomatic and safe -- so `(d/q query
  db)` is not evidence of anything, and treating it as a sink accuses most of
  the data layer.

  A SQL statement is a string. A string statement handed in from elsewhere is
  the cross-file injection shape, and a bare symbol is what that looks like at
  the sink. `dangerous-sinks` still covers the case where the building happens
  in the same form, including for Datalog.

  Without argument-position tracking this pass cannot afford a loose sink set:
  it would report any var that reads a request and calls any data function."
  [nodes]
  (for [l (tree/lists-headed-by nodes sql-fns)
        :let [a (statement-arg nodes l)]
        :when (and a (= :symbol (:type a)) (not (:quoted? a)))]
    l))

(defn seeds
  "Which vars directly obtain attacker-influenced data, and which hand data to
  an UNSAFE sink. These seed net.typemark.sift.callgraph."
  [nodes]
  (let [nsname (namespace-name nodes)
        var-of (fn [l] (when-let [v (enclosing-var nodes l)] [nsname v]))]
    (if-not nsname
      {:taints #{} :reaches #{}}
      {:taints  (into #{} (keep var-of)
                      (concat (tree/lists-headed-by nodes sources)
                              (get-in-sources nodes)))
       :reaches (into #{} (keep var-of)
                      (concat (dangerous-sinks nodes)
                              (reaching-sinks nodes)))})))

(defn seeds-of-source [source]
  (let [{:keys [ok? nodes]} (parse/parse source)]
    (when ok? (seeds nodes))))

(defn findings-of-source [source]
  (let [{:keys [ok? nodes]} (parse/parse source)]
    (when ok? (vec (findings nodes)))))
