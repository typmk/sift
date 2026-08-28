(ns com.typemark.sift.parse
  "Parses Clojure source with rewrite-clj and flattens the tree into the
  annotated node stream every other namespace consumes.

  rewrite-clj rather than a hand-written lexer because it keeps whitespace
  and comments as nodes, so `#_` and `(comment ...)` are recognisable as
  structure instead of guessed at, and head position is a tree fact rather
  than a stack heuristic.

  NOT tools.analyzer: that resolves and macroexpands, which means loading the
  namespace. A scanner that evaluates the code it is measuring is a scanner
  that runs your side effects on the CI box.

  Positions are 1-based with `end-col` exclusive, matching both rewrite-clj
  and clj-kondo. Conversion to Sonar's 0-based offsets happens at the edge."
  (:require [clojure.string :as str]
            [com.typemark.sift.forms :as forms]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]))

#?(:clj (set! *warn-on-reflection* true))

(def ^:private trivia #{:whitespace :newline :comma})

(defn- classify
  "A rewrite-clj :token covers symbols, keywords, numbers, strings and chars.
  Split them on the text image."
  [tag text]
  (case tag
    :comment :comment
    (:whitespace :newline :comma) :trivia
    :regex :regex
    :multi-line :string
    :token (cond
             (str/starts-with? text "\"") :string
             (str/starts-with? text "\\") :char
             (str/starts-with? text ":")  :keyword
             (re-matches #"[-+]?[0-9].*" text) :number
             :else :symbol)
    :structure))

(defn- head-text
  "The first non-trivia child of a list, as text, or nil."
  [node]
  (when (n/inner? node)
    (some->> (n/children node)
             (remove #(contains? trivia (n/tag %)))
             first
             n/string)))

(defn- pos [node]
  (let [m (meta node)]
    (when (:row m)
      {:line (:row m) :col (:col m)
       :end-line (:end-row m) :end-col (:end-col m)})))

(defn- walk
  [node depth commented? quoted? branch-depth acc]
  (let [tag    (n/tag node)
        inner? (n/inner? node)
        p      (pos node)
        text   (n/string node)
        list?  (contains? #{:list :fn} tag)
        head   (when list? (head-text node))
        commented?' (or commented?
                        (= :uneval tag)
                        (and list? (= "comment" head)))
        quoted?'    (or quoted? (contains? #{:quote :syntax-quote} tag))
        branch?     (and list? (contains? forms/branch head))
        acc' (if p
               (conj! acc (assoc p
                                 :tag tag
                                 :inner? inner?
                                 :type (classify tag text)
                                 :text text
                                 :depth depth
                                 :commented? commented?'
                                 :quoted? quoted?'
                                 :head head
                                 :branch? branch?
                                 :function? (and list? (contains? forms/function head))
                                 :class? (and list? (contains? forms/type-def head))
                                 :branch-nesting branch-depth))
               acc)]
    (if inner?
      (reduce (fn [a c] (walk c (inc depth) commented?' quoted?'
                              (cond-> branch-depth branch? inc) a))
              acc' (n/children node))
      acc')))

(defn parse
  "Source -> {:ok? true :nodes [...]} or {:ok? false :error msg}.

  A parse failure means the file is not valid Clojure, which clj-kondo will
  have reported as a syntax finding. The sensor says so rather than silently
  contributing zero lines to ncloc."
  [source]
  (try
    {:ok? true
     :nodes (persistent! (walk (p/parse-string-all source) 0 false false 0 (transient [])))}
    ;; :default rather than js/Error on the cljs side: a reader error can be
    ;; thrown as a plain value, and catching only js/Error would let it escape
    ;; as an uncaught exception rather than becoming {:ok? false}.
    (catch #?(:clj Exception :cljs :default) e
      {:ok? false :error #?(:clj (.getMessage e) :cljs (ex-message e))})))

(defn leaves
  "Nodes with no children: what highlighting and duplication are computed
  over. Asked of the tree, not of a hand-kept list of container tags that
  would have to track every node type rewrite-clj adds."
  [nodes]
  (remove :inner? nodes))

(def ^:private literal-types #{:string :regex :char :number})

(defn cpd-image
  "The image a duplication token is matched on. Literals collapse to a
  placeholder so two blocks differing only in their constants still count as
  duplicated -- which is the whole point of looking for them."
  [{:keys [type text]}]
  (if (contains? literal-types type)
    (str "$" (str/upper-case (name type)))
    text))
