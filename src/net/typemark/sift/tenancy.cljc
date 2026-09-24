(ns net.typemark.sift.tenancy
  (:require [net.typemark.sift.tree :as tree]))

(def ^:private two-valued #{"if" "when" "when-not" "if-not" "nil?" "some?" "if-let" "when-let"})

(def ^:private query-fns
  #{"d/q" "datomic/q" "q" "jdbc/execute!" "jdbc/query" "sql/query" "execute!"})

(def ^:private defining-heads
  #{"defrecord" "deftype" "extend-type" "extend-protocol" "reify" "defprotocol"})

(def shared-namespaces
  #{"db" "datomic" "attr" "schema"})

(def ^:private denial
  #"(?i)^(throw|ex-info|deny|denied|forbidden|unauthorized|unauthorised|abort|reject)")

(defn- only-shared?
  [nodes l shared]
  (let [ks (for [k (tree/children-of nodes l)
                 :when (= :keyword (:type k))
                 :let [m (re-find #"^:([^/]+)/" (str (:text k)))]
                 :when m]
             (second m))]
    (and (seq ks) (every? shared ks))))

(defn- denies?
  [nodes n]
  (let [encl (->> nodes
                  (filter #(and (contains? tree/call-tags (:tag %))
                                (<= (:line %) (:line n))
                                (>= (:end-line %) (:end-line n))))
                  (sort-by :depth) first)]
    (boolean (some #(and (:text %) (re-find denial (:text %)))
                   (tree/children-of nodes (or encl n))))))

(defn- mentions? [nodes n re]
  (boolean (some #(and (:text %) (re-find re (:text %)))
                 (cons n (tree/children-of nodes n)))))

(defn ambiguous-owner-check
  [nodes tenant]
  (for [l (tree/lists-headed-by nodes two-valued)
        :let [a (tree/first-argument nodes l)]
        :when (and a (mentions? nodes a tenant) (denies? nodes l))]
    (tree/hit l (str "two-valued test on the tenant boundary: nil owner means both"
                     " untenanted and unresolved, and both would take this branch"))))

(defn unscoped-tenant-query
  [nodes tenant shared]
  (let [defining (into #{} (mapcat #(map :text (tree/children-of nodes %)))
                       (tree/lists-headed-by nodes defining-heads))]
    (for [l (tree/lists-headed-by nodes query-fns)
          :when (not (mentions? nodes l tenant))
          :when (not (only-shared? nodes l (into shared-namespaces shared)))
          :when (not (and (contains? defining (:head l))
                          (some #(and (= :list (:tag %))
                                      (<= (:line %) (:line l))
                                      (>= (:end-line %) (:end-line l))
                                      (contains? (disj defining-heads "reify") (:head %)))
                                nodes)))]
      (tree/hit l (str (:head l) " names no owner or tenant -- confirm the scope is in the"
                       " query and not applied afterwards")))))
