(ns net.typemark.sift.kondo
  (:require [clj-kondo.hooks-api :as api]
            [net.typemark.sift.portable.complexity :as complexity]
            [net.typemark.sift.portable.cond-case :as cond-case]
            [net.typemark.sift.portable.prose :as prose]
            [net.typemark.sift.portable.shape :as shape]))

(def ^:private defining '#{defn defn- fn fn* defmacro defmethod})

(def ^:private unit-heads
  '#{defn defn- defmacro fn fn* defmethod letfn defrecord deftype reify extend-protocol
     extend-type specify! proxy extend def defonce})

(def ^:private doc-heads '#{defn defn- defmacro defmulti def defonce defprotocol})

(defn- head-of [node]
  (let [h (some-> (first (:children node)) api/sexpr)]
    (when (symbol? h) (symbol (name h)))))

(defn- text [n]
  (try (pr-str (api/sexpr n)) (catch Exception _ "")))

(def ^:private ops
  {:tag api/tag
   :text text
   :inner? (fn [n] (some? (:children n)))
   :children :children
   :fn-params (fn [n]
                (count (distinct (mapcat #(re-seq #"%\d*&?" %)
                                         (keep #(when (= :token (api/tag %)) (text %))
                                               (tree-seq :children :children n))))))})

(defn- inside-defn? []
  (some #(contains? defining (:name %)) (api/callstack)))

(defn- report! [node type hit]
  (when hit
    (api/reg-finding! (assoc (meta node) :message (:message hit) :type type))))

(defn cond-as-case
  [{:keys [node]}]
  (when (inside-defn?)
    (report! node :sift/cond-as-case (cond-case/check (api/sexpr node))))
  nil)

(defn loop-form
  [{:keys [node]}]
  (when (inside-defn?)
    (let [form (api/sexpr node)]
      (report! node :sift/loop-as-map (shape/loop-as-map form))
      (report! node :sift/loop-as-reduce (shape/loop-as-reduce form))))
  nil)

(defn let-form
  [{:keys [node config]}]
  (when (inside-defn?)
    (let [form (api/sexpr node)
          lint (:linters config)]
      (report! node :sift/cond-as-build-up (shape/cond-as-build-up form (get-in lint [:sift/cond-as-build-up :min] 3)))
      (report! node :sift/let-as-thread (shape/let-as-thread form (get-in lint [:sift/let-as-thread :min] 3)))))
  nil)

(defn- cognitive-complexity
  [{:keys [node config filename]} stack]
  (when (and (contains? unit-heads (head-of node))
             (not-any? #(contains? unit-heads (:name %)) stack))
    (let [max-score (get-in config [:linters :sift/cognitive-complexity :max] 15)
          {:keys [ok? functions]} (complexity/report-forms ops [node] (complexity/features-for filename))]
      (when ok?
        (doseq [u (complexity/flatten-units functions)
                :when (> (:cognitive u) max-score)]
          (api/reg-finding! {:row (:line u) :col 1 :end-row (:line u) :end-col 2
                             :type :sift/cognitive-complexity
                             :message (str (:name u) " has cognitive " (:cognitive u) " (max " max-score
                                           "); cyclomatic " (:cyclomatic u) ", nesting " (:max-nesting u))}))))))

(defn- at [rec doc-node]
  (let [{:keys [row col end-row end-col]} (meta doc-node)]
    (assoc rec :line row :column col :end-line end-row :end-column end-col)))

(defn- docstrings
  [{:keys [node config]}]
  (let [h (head-of node)
        form (when (contains? doc-heads h) (api/sexpr node))
        recs (cond
               (= 'defprotocol h)
               (for [[i rec] (prose/protocol-records form)]
                 (at rec (last (:children (nth (:children node) i)))))
               form
               (when-let [rec (prose/var-record form)]
                 [(at rec (nth (:children node) 2))]))
        {:keys [min-words min-overlap] :or {min-words 3 min-overlap 0.75}}
        (get-in config [:linters :sift/narrates-body])]
    (doseq [h (concat (prose/findings recs) (prose/narrates-body recs min-words min-overlap))]
      (api/reg-finding! {:row (:line h) :col (:column h) :end-row (:end-line h) :end-col (:end-column h)
                         :type (keyword "sift" (name (:rule h)))
                         :message (:message h)}))))

(defn definition
  [{:keys [cljc lang] :as ctx}]
  (let [stack (api/callstack)]
    (when (and (not (and cljc (= :cljs lang)))
               (not-any? #(= 'comment (:name %)) stack))
      (cognitive-complexity ctx stack)
      (docstrings ctx)))
  nil)
