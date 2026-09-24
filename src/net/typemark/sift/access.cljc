(ns net.typemark.sift.access
  (:require [clojure.string :as str]
            [net.typemark.sift.tree :as tree]))

(def ^:private tenant-word
  #"(?i)(^|[-_*/:.])(owner|org|org-id|orgid|tenant|tenant-id|party-uuid)([-_*?!/]|$)")

(def ^:private two-valued #{"if" "when" "when-not" "if-not" "nil?" "some?" "if-let" "when-let"})

(def ^:private query-fns
  #{"d/q" "datomic/q" "q" "jdbc/execute!" "jdbc/query" "sql/query" "execute!"})

(def shared-ns
  #{"db" "datomic" "attr" "schema"})

(defn- only-shared?
  [shared nodes l]
  (let [ks (for [k (tree/children-of nodes l)
                 :when (= :keyword (:type k))
                 :let [m (re-find #"^:([^/]+)/" (str (:text k)))]
                 :when m]
             (second m))]
    (and (seq ks) (every? shared ks))))

(def ^:private denial
  #"(?i)^(throw|ex-info|deny|denied|forbidden|unauthorized|unauthorised|abort|reject)")

(defn- denies?
  [nodes n]
  (let [encl (->> nodes
                  (filter #(and (contains? tree/call-tags (:tag %))
                                (<= (:line %) (:line n))
                                (>= (:end-line %) (:end-line n))))
                  (sort-by :depth) first)]
    (boolean (some #(and (:text %) (re-find denial (:text %)))
                   (tree/children-of nodes (or encl n))))))

(defn tenanted?
  [texts]
  (boolean (some #(and (string? %)
                       (some (fn [t] (re-find tenant-word t))
                             (str/split % #"[\s()\[\]{}\"'`,;@^~]+")))
                 texts)))

(defn- mentions? [nodes n re]
  (boolean (some #(and (:text %) (re-find re (:text %)))
                 (cons n (tree/children-of nodes n)))))

(defn findings
  [nodes & {shared-ns' :shared-ns :keys [tenanted?] :or {tenanted? true}}]
  (concat
   (for [l (tree/lists-headed-by nodes two-valued)
         :let [a (tree/first-argument nodes l)]
         :when (and a (mentions? nodes a tenant-word) (denies? nodes l))]
     {:rule "ambiguous-owner-check"
      :line (:line l) :col (:col l) :end-line (:end-line l) :end-col (:end-col l)
      :message (str "two-valued test on the tenant boundary: nil owner means both"
                    " untenanted and unresolved, and both would take this branch")})

   (let [defining (into #{} (mapcat #(map :text (tree/children-of nodes %)))
                        (tree/lists-headed-by nodes #{"defrecord" "deftype" "extend-type"
                                                      "extend-protocol" "reify" "defprotocol"}))]
     (for [l (tree/lists-headed-by nodes query-fns)
           :when tenanted?
           :when (not (mentions? nodes l tenant-word))
           :when (not (only-shared? (into shared-ns (map name) shared-ns') nodes l))
           :when (not (and (contains? defining (:head l))
                           (some #(and (contains? #{:list} (:tag %))
                                       (<= (:line %) (:line l))
                                       (>= (:end-line %) (:end-line l))
                                       (contains? #{"defrecord" "deftype" "extend-type"
                                                    "extend-protocol" "defprotocol"} (:head %)))
                                 nodes)))]
       {:rule "unscoped-tenant-query"
        :line (:line l) :col (:col l) :end-line (:end-line l) :end-col (:end-col l)
        :message (str (:head l) " names no owner or tenant -- confirm the scope is in the"
                      " query and not applied afterwards")}))))
