(ns net.typemark.sift.kondo
  (:require [clj-kondo.hooks-api :as api]
            [net.typemark.sift.portable.complexity :as complexity]
            [net.typemark.sift.portable.cond-case :as cond-case]))

(def ^:private defining '#{defn defn- fn fn* defmacro defmethod})

(defn- inside-defn? []
  (boolean (some #(contains? defining (:name %)) (api/callstack))))

(defn cond-as-case
  [{:keys [node]}]
  (when (inside-defn?)
    (when-let [hit (cond-case/check (api/sexpr node))]
      (api/reg-finding! (assoc (meta node) :message (:message hit) :type :sift/cond-as-case))))
  nil)

(def ^:private unit-heads
  '#{defn defn- defmacro fn fn* defmethod letfn defrecord deftype reify extend-protocol
     extend-type specify! proxy extend def defonce})

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

(defn cognitive-complexity
  [{:keys [node config cljc lang filename]}]
  (let [stack (api/callstack)]
    (when (and (not (and cljc (= :cljs lang)))
               (not-any? #(contains? unit-heads (:name %)) stack)
               (not-any? #(= 'comment (:name %)) stack))
      (let [max-score (get-in config [:linters :sift/cognitive-complexity :max] 15)
            {:keys [ok? functions]} (complexity/report-forms ops [node] (complexity/features-for filename))]
        (when ok?
          (doseq [u (complexity/flatten-units functions)
                  :when (> (:cognitive u) max-score)]
            (api/reg-finding! {:row (:line u) :col 1 :end-row (:line u) :end-col 2
                               :type :sift/cognitive-complexity
                               :message (str (:name u) " has cognitive " (:cognitive u) " (max " max-score
                                             "); cyclomatic " (:cyclomatic u) ", nesting " (:max-nesting u))}))))))
  nil)
