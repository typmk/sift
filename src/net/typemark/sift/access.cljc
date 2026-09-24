(ns net.typemark.sift.access
  "Rules for multi-tenant access: queries scoped by an owner or tenant.

  Java fills CWE-285 and CWE-287 with rules about Spring Security
  annotations. The vulnerability class is the same and the vocabulary is not:
  in a Datomic or SQL codebase authorisation is scoping a query by owner,
  org or tenant, and nothing enforces it.

  The motivating defect is real and was shipped: a nil owner meant BOTH
  \"untenanted, shared by design\" and \"lookup failed\" -- and both branches
  allowed. A two-valued test on a three-valued question.

  Everything here is a hotspot rather than a vulnerability. The shapes are
  strong signals, not proofs, and a wrong accusation about authorisation is
  the fastest way to get a security ruleset switched off."
  (:require [clojure.string :as str]
            [net.typemark.sift.tree :as tree]))

(def ^:private tenant-word
  "For a QUERY: does it name the scope anywhere. The trailing boundary must
  admit `/`, or `:tenant/id` -- the actual scoping attribute here -- does not
  match and correctly scoped queries read as unscoped."
  #"(?i)(^|[-_*/:.])(owner|org|org-id|orgid|tenant|tenant-id|party-uuid)([-_*?!/]|$)")

(def ^:private two-valued #{"if" "when" "when-not" "if-not" "nil?" "some?" "if-let" "when-let"})

(def ^:private query-fns
  "Scans. `pull` and `d/pull` are deliberately absent: a pull takes an entity
  id the caller already holds, so it is not an unscoped search -- the tenancy
  question belongs where that id came from. They were 40 of 124 findings on
  three services, and not one of them was a scan."
  #{"d/q" "datomic/q" "q" "jdbc/execute!" "jdbc/query" "sql/query" "execute!"})

(def shared-ns
  "Attribute namespaces that are SHARED rather than tenant-scoped, whatever
  the project: the schema itself. A project adds its own reference layer --
  a taxonomy, a units table -- as `:shared-ns` on `findings`. A query
  touching only these has no tenant to name.

  Measured across three services: the reference layer was the single largest
  false-positive class (75 attribute mentions among the findings), and
  counting rows in a shared taxonomy is not a tenancy bug.

  Closed list, and the check is inverted against it: a query mentioning
  anything NOT in here is still flagged, so a new tenanted attribute is
  covered by default and only the shared layer has to be enumerated."
  #{"db" "datomic" "attr" "schema"})

(defn- only-shared?
  "True when every namespaced attribute the query names is in the shared
  layer -- and false when it names none at all, because a query with no
  attributes says nothing either way and the safer reading is to keep it."
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
  "True when this branch, or the form containing it, can refuse. An
  authorisation check that cannot deny is not an authorisation check."
  [nodes n]
  (let [encl (->> nodes
                  (filter #(and (contains? tree/call-tags (:tag %))
                                (<= (:line %) (:line n))
                                (>= (:end-line %) (:end-line n))))
                  (sort-by :depth) first)]
    (boolean (some #(and (:text %) (re-find denial (:text %)))
                   (tree/children-of nodes (or encl n))))))

(defn tenanted?
  "Does any of these source texts say owner, org or tenant at all? A corpus
  that never does has no tenant to scope a query by — a single-user ledger
  library, five of five findings wrong — and `unscoped-tenant-query` is
  vacuous over it. Judged over the CORPUS, not the file: a one-query
  fixture says nothing about tenancy and must still fire."
  [texts]
  ;; per token, as `mentions?` asks it: tenant-word anchors on a token's
  ;; start and end, so over a whole file `:obj/owner ?o]` never matched
  (boolean (some #(and (string? %)
                       (some (fn [t] (re-find tenant-word t))
                             (str/split % #"[\s()\[\]{}\"'`,;@^~]+")))
                 texts)))

(defn- mentions? [nodes n re]
  (boolean (some #(and (:text %) (re-find re (:text %)))
                 (cons n (tree/children-of nodes n)))))

(defn findings
  "`:tenanted? false` — from `tenanted?` over the corpus — switches the
  query rule off; the default keeps it on. `:shared-ns` adds a project's own
  shared attribute namespaces to `shared-ns`."
  [nodes & {shared-ns' :shared-ns :keys [tenanted?] :or {tenanted? true}}]
  (concat
   (for [l (tree/lists-headed-by nodes two-valued)
         :let [a (tree/first-argument nodes l)]
         :when (and a (mentions? nodes a tenant-word) (denies? nodes l))]
     {:rule "ambiguous-owner-check"
      :line (:line l) :col (:col l) :end-line (:end-line l) :end-col (:end-col l)
      :message (str "two-valued test on the tenant boundary: nil owner means both"
                    " untenanted and unresolved, and both would take this branch")})

   ;; A vacuous rule is refused rather than run — the boundary checker's
   ;; rule — and `tenanted?` over the corpus is what decides.
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
