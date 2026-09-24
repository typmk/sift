(ns net.typemark.sift.security
  (:require [net.typemark.sift.parse :as parse]
            [net.typemark.sift.tree :as tree]))

(def ^:private sources
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
  [nodes lst]
  (->> (tree/children-of nodes lst)
       (remove #(= :trivia (:type %)))
       (remove #(= (:head lst) (:text %)))
       (filter #(contains? #{:symbol :list} (if (= :list (:tag %)) :list (:type %))))
       (some #(not (tree/literal? %)))
       boolean))

(defn- world-accessible?
  [s]
  (boolean
   (and s
        (or (and (re-matches #"0?[0-7]{3}" s)
                 (pos? (bit-and (parse-long (str (last s))) 2r011)))
            (re-matches #"[-dlbcps][-r][-w][-xsS][-r][-w][-xsS][-r][-w][-xtT]" s)
            (re-matches #"[-r][-w][-xsS][-r][-w][-xsS][-r][-w][-xtT]" s)))))

(defn findings
  [nodes]
  (concat
   (for [l (tree/lists-headed-by nodes shell-fns)
         :when (not (any-dynamic-arg? nodes l))]
     (finding "shell-invocation" l
              "shell invocation -- confirm no argument is caller-controlled"))

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
                    (not (re-find #"(?i)[-_](type|kind|name|id|header|field|param|path|env|key-name)$" nm-text)))]
     (finding "hardcoded-credential" v
              (str "credential-shaped name '" nm-text "' is bound to a literal")))

   (for [l (tree/lists-headed-by nodes xml-fns)]
     (finding "xml-external-entity" l
              "confirm this parser has external entity resolution disabled"))

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
  [nodes n]
  (->> (tree/lists-headed-by nodes #{"defn" "defn-" "def" "defmacro" "defmethod"})
       (filter #(and (<= (:line %) (:line n)) (>= (:end-line %) (:line n))))
       (sort-by #(- (:end-line %) (:line %)))
       first
       (#(when % (tree/first-argument nodes %)))
       (#(when % (tree/text nodes %)))))

(defn- dangerous-sinks
  [nodes]
  (concat
   (filter #(dynamic-arg? nodes %) (tree/lists-headed-by nodes eval-fns))
   (tree/lists-headed-by nodes read-string-fns)
   (filter #(any-dynamic-arg? nodes %) (tree/lists-headed-by nodes shell-fns))
   (filter (fn [l] (some #(and (= :list (:tag %)) (contains? build-fns (:head %)))
                         (tree/children-of nodes l)))
           (tree/lists-headed-by nodes sql-fns))))

(defn- get-in-sources
  [nodes]
  (for [l (tree/lists-headed-by nodes #{"get-in" "get"})
        :when (some #(and (= :keyword (:type %)) (contains? sources (:text %)))
                    (tree/children-of nodes l))]
    l))

(defn- statement-arg
  [nodes l]
  (when-let [v (some (fn [x] (when (= :vector (:tag x)) x)) (tree/children-of nodes l))]
    (some (fn [x] (when (= (inc (:depth v)) (:depth x)) x)) (tree/children-of nodes v))))

(defn- reaching-sinks
  [nodes]
  (for [l (tree/lists-headed-by nodes sql-fns)
        :let [a (statement-arg nodes l)]
        :when (and a (= :symbol (:type a)) (not (:quoted? a)))]
    l))

(defn seeds
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
