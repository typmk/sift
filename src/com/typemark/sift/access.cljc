(ns com.typemark.sift.access
  "Rules for the access kernel, keyed on this codebase's own vocabulary.

  Java fills CWE-285 and CWE-287 with rules about Spring Security
  annotations. The vulnerability class is the same and the vocabulary is not:
  here authorisation is `:obj/owner` scoping, a grant lattice and an operator
  that must never be a party. Those invariants are stated in CLAUDE.md and
  enforced by nothing.

  The motivating defect is real and was shipped: WEB-184, where a nil owner
  meant BOTH \"untenanted, shared by design\" and \"lookup failed\" -- and
  both branches allowed. A two-valued test on a three-valued question. No
  general-purpose analyzer can find that, because only this codebase knows
  that `:obj/owner` is the tenant boundary.

  Everything here is a hotspot rather than a vulnerability. The shapes are
  strong signals, not proofs, and a wrong accusation about authorisation is
  the fastest way to get a security ruleset switched off."
  (:require [com.typemark.sift.tree :as tree]))

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
  lume, sur and forma, and not one of them was a scan."
  #{"d/q" "datomic/q" "q" "jdbc/execute!" "jdbc/query" "sql/query" "execute!"})

(def ^:private shared-attr-ns
  "Attribute namespaces the dictionary defines as SHARED rather than
  org-scoped: the taxonomy and compat layer classifies and relates product
  DEFINITIONS, and is the same for every tenant. A query touching only these
  has no tenant to name.

  Measured across lume, sur and forma: std 25, db 25, conn 13 and taxon 12
  attribute mentions among the findings -- the reference layer was the single
  largest false-positive class, and counting rows in a shared taxonomy is not
  a tenancy bug.

  Closed list, and the check is inverted against it: a query mentioning
  anything NOT in here is still flagged, so a new tenanted attribute is
  covered by default and only the shared layer has to be enumerated."
  #{"taxon" "std" "conn" "line" "function" "facet" "measurement" "model"
    "compat" "db" "datomic" "attr" "schema"})

(defn- only-shared?
  "True when every namespaced attribute the query names is in the shared
  layer -- and false when it names none at all, because a query with no
  attributes says nothing either way and the safer reading is to keep it."
  [nodes l]
  (let [ks (for [k (tree/children-of nodes l)
                 :when (= :keyword (:type k))
                 :let [m (re-find #"^:([^/]+)/" (str (:text k)))]
                 :when m]
             (second m))]
    (and (seq ks) (every? shared-attr-ns ks))))

(def ^:private banned-operator-attrs
  "Named in CLAUDE.md's `Banned -> use` table: operator access is an
  off-graph staff fact, never a party attribute."
  #{":party/platform-role" ":party/operator" ":party/staff-role" ":party/is-operator"})

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
  that never does has no tenant to scope a query by — agentia's single-user
  ledger, five of five findings wrong — and `unscoped-tenant-query` is
  vacuous over it. Judged over the CORPUS, not the file: a one-query
  fixture says nothing about tenancy and must still fire, and sonar-clojure's
  rule tests say so."
  [texts]
  (boolean (some #(and (string? %) (re-find tenant-word %)) texts)))

(defn- mentions? [nodes n re]
  (boolean (some #(and (:text %) (re-find re (:text %)))
                 (cons n (tree/children-of nodes n)))))

(defn findings
  "`:tenanted? false` — from `tenanted?` over the corpus — switches the
  query rule off; the default keeps it on, which is the contract sonar's
  sensor and its fixtures hold."
  [nodes & {:keys [tenanted?] :or {tenanted? true}}]
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
           :when (not (only-shared? nodes l))
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
                      " query and not applied afterwards")}))

   (for [n nodes
         :when (and (= :keyword (:type n)) (not (:commented? n))
                    (contains? banned-operator-attrs (:text n)))]
     {:rule "operator-as-party"
      :line (:line n) :col (:col n) :end-line (:end-line n) :end-col (:end-col n)
      :message (str (:text n) " puts operator access on the graph; it is an off-graph"
                    " staff fact, and blindness is enforced by absence")})))
