(ns net.typemark.sift.parse
  (:require [clojure.string :as str]
            [net.typemark.sift.forms :as forms]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]))

#?(:clj (set! *warn-on-reflection* true))

(def ^:private trivia #{:whitespace :newline :comma})

(defn- classify
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
  [node depth commented? quoted? branch-depth parent acc]
  (let [tag    (n/tag node)
        inner? (n/inner? node)
        p      (pos node)
        text   (when-not inner? (n/string node))
        list?  (contains? #{:list :fn} tag)
        head   (when list? (head-text node))
        commented?' (or commented?
                        (= :uneval tag)
                        (and list? (= "comment" head)))
        quoted?'    (or quoted? (contains? #{:quote :syntax-quote} tag))
        branch?     (and list? (contains? forms/branch head))
        i    (count acc)
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
                                 :branch-nesting branch-depth
                                 :index i
                                 :last i
                                 :parent parent))
               acc)]
    (if inner?
      (let [acc'' (reduce (fn [a c] (walk c (inc depth) commented?' quoted?'
                                          (cond-> branch-depth branch? inc) (when p i) a))
                          acc' (n/children node))
            end   (dec (count acc''))]
        (if (and p (> end i))
          (assoc! acc'' i (assoc (nth acc'' i) :last end))
          acc''))
      acc')))

(defn- line-starts
  [s]
  (loop [from 0 acc [0]]
    (if-let [i (str/index-of s "\n" from)]
      (recur (inc (long i)) (conj acc (inc (long i))))
      acc)))

(defn nodes-of
  [root]
  (let [source (n/string root)]
    (with-meta (persistent! (walk root 0 false false 0 nil (transient [])))
               {::source source ::line-starts (delay (line-starts source))})))

(defn parse-root
  [source]
  (try
    (let [root (p/parse-string-all source)]
      {:ok? true :root root :nodes (nodes-of root)})
    (catch #?(:clj Exception :cljs :default) e
      {:ok? false :error #?(:clj (.getMessage e) :cljs (ex-message e))})))

(defn parse
  [source]
  (dissoc (parse-root source) :root))

(defn leaves
  [nodes]
  (remove :inner? nodes))

(def ^:private literal-types #{:string :regex :char :number})

(defn cpd-image
  [{:keys [type text]}]
  (if (contains? literal-types type)
    (str "$" (str/upper-case (name type)))
    text))
