(ns com.typemark.sift.callgraph
  "Interprocedural taint propagation over clj-kondo's call graph.

  clj-kondo's analysis emits, for every var usage, the var it appeared in
  (`from-var`) and the var it refers to (`to`/`name`). That is a call graph,
  and it is what lifts taint tracking above the single-form case the
  intraprocedural pass in `com.typemark.sift.security` is limited to.

  The propagation is a fixpoint over two relations:

    taints  -- a var whose RETURN carries attacker-influenced data, either
               because it calls a source directly or because it calls
               something that taints
    reaches -- a var that passes a value to a sink, directly or through a
               callee that does

  A var that both taints and reaches is a path from source to sink. Reported
  as one finding on the calling var, with a flow through the chain.

  What it still does NOT do: argument-position tracking. If `f` taints and
  `g` reaches, and some third var calls both, this reports it -- even if the
  tainted value never actually reached the sink argument. That is a
  deliberate over-approximation, and it is why these findings are reported at
  a lower confidence than the direct case rather than mixed in with it."
  (:require [com.typemark.sift.json :as json]))

(def ^:private js-max-row
  "An open-ended region runs to the end of the file. A literal rather than
  Long/MAX_VALUE, which does not exist in ClojureScript -- this namespace
  compiles to CLJS inside defnet, the same reason `format` is banned here."
  1000000)

(defn- row-of [m] (or (get m "row") (get m "name-row")))

(def ^:private computed-dispatch
  "What an arm is called when its dispatch value is not a literal. The same
  string defnet's parser uses, so the two name the same arm."
  "<computed>")

(defn- literal-dispatch?
  "Whether this `dispatch-val-str` is a value rather than a form to evaluate.

  defnet decides this on the tree-sitter node type; kondo gives text only, so
  this reads the text. A string literal is admitted whole, because its contents
  may spell anything; otherwise a form, a set, a map or a reader macro is not a
  name. `[::a ::b]` stays a name -- vector dispatch is `isa?` dispatch, and
  defnet admits it too."
  [d]
  (and (seq d)
       (or (= \" (first d))
           (not (some #{\( \) \{ \} \~ \@ \` \#} d)))))

(defn- arm-name
  "The name of one multimethod arm, or nil for any other usage.

  clj-kondo marks the usage of the multimethod inside each `defmethod` with
  `defmethod` and `dispatch-val-str`. A keyword dispatch value carries its own
  colon and the separator supplies one, so it is stripped -- the result equals
  the node name defnet's parser gives the same arm.

  A dispatch value that is a FORM names nothing, and taking its text produced
  `handle::(:k m)`, a node shaped like an arm that names no arm. defnet stopped
  minting those on 2026-09-17 and calls them `<computed>`; this follows, or the
  two disagree about exactly the arms that are hardest to see. MEASURED on
  lume's analysis: 42 defmethod-marked usages, 40 literal and 2 not, both
  `(:flaky ops)`."
  [u]
  (when (get u "defmethod")
    (let [d (get u "dispatch-val-str")
          d (if (and d (= \: (first d))) (subs d 1) d)]
      (when (seq d)
        (str (get u "name") "::" (if (literal-dispatch? d) d computed-dispatch))))))

(defn- owner-regions
  "The spans clj-kondo attributes no `from-var` to, and who owns each.

  THIS IS THE WHOLE POINT OF THE NAMESPACE HOLDING TOGETHER. `from-var`
  attributes a usage to a VAR, and neither a protocol implementation nor a
  multimethod arm defines one -- so every call in those bodies arrived with
  `from-var` absent and was dropped on the floor. In Clojure that is precisely
  where polymorphic dispatch lands: an `-render` that interpolates a request
  value, a `(defmethod handle :upload)` that writes a file. The taint graph
  reported clean because it never saw the node. Measured on defnet's own src:
  284 orphan usages inside protocol-impl spans and all 31 multimethod arms.

  Two shapes, and they are not equally precise:

    protocol impls -- EXACT. `:protocol-impls` gives a real row span, and
                      covers deftype, defrecord, reify, extend-type and
                      extend-protocol.
    multimethod arms -- BOUNDED, not exact. The marker usage spans the
                      multimethod's name symbol only, so the arm's extent is
                      taken as running to the next region or var definition in
                      the same file. A top-level form sitting between two arms
                      is therefore attributed to the earlier arm. That is an
                      over-approximation of the same kind this namespace
                      already declares for argument positions, and it errs
                      toward a false positive rather than the false negative
                      that dropping the call outright guarantees."
  [a]
  (let [impls (for [p (get a "protocol-impls" [])]
                {:file (get p "filename")
                 :from (get p "row")
                 :to   (get p "end-row")
                 :owner [(get p "impl-ns")
                         (str (get p "protocol-name") "/" (get p "method-name"))]
                 :via  [(get p "protocol-ns") (get p "method-name")]})
        ;; The arm belongs to the MULTIMETHOD's namespace, not the file's. A
        ;; defmethod in another file extends that multi, so its identity lives
        ;; where the defmulti does -- which is also how defnet names it.
        arms  (for [u (get a "var-usages" [])
                    :let [nm (arm-name u)]
                    :when nm]
                {:file (get u "filename")
                 :from (row-of u)
                 :owner [(get u "to") nm]
                 :via  [(get u "to") (get u "name")]})
        starts (reduce (fn [m x]
                         (if (and (:file x) (:from x))
                           (update m (:file x) (fnil conj []) (:from x))
                           m))
                       {}
                       (concat impls arms
                               (for [d (get a "var-definitions" [])]
                                 {:file (get d "filename") :from (row-of d)})))
        sorted (into {} (for [[f rs] starts] [f (vec (sort rs))]))]
    (vec (for [x (concat impls arms)]
           (assoc x :to (or (:to x)
                            (if-let [nxt (first (drop-while #(<= % (:from x))
                                                            (get sorted (:file x))))]
                              (dec nxt)
                              js-max-row)))))))

(defn- owner-of
  "The innermost region containing this position, or nil."
  [regions file row]
  (when (and file row)
    (->> regions
         (filter #(and (= file (:file %))
                       (<= (:from %) row)
                       (<= row (:to %))))
         (sort-by :from >)
         first)))

(defn call-graph
  "analysis JSON -> {[ns var] #{[callee-ns callee-var]}} plus the position of
  each call site.

  A usage with no `from-var` is attributed to the protocol implementation or
  multimethod arm containing it rather than discarded -- see `owner-regions`.
  Each region also gains an incoming edge from the name a CALLER writes: the
  protocol method var, or the multimethod. Without it an implementation is an
  island that can only be tainted by calling a source itself, and the flow
  from a handler through a polymorphic call into the body that answers it does
  not exist."
  [analysis-text]
  (let [a (get (json/read-str analysis-text) "analysis")
        regions (owner-regions a)
        g (reduce
           (fn [g u]
             (let [from (get u "from-var")
                   fns' (get u "from")
                   to   (get u "to")
                   nm   (get u "name")
                   owner (cond
                           (and from fns') [fns' from]
                           (get u "defmethod") nil
                           :else (:owner (owner-of regions (get u "filename")
                                                   (row-of u))))]
               (if (and owner to nm)
                 (update g owner (fnil conj #{})
                         {:callee [to nm]
                          :filename (get u "filename")
                          :line (get u "name-row") :col (get u "name-col")
                          :end-line (get u "name-end-row") :end-col (get u "name-end-col")})
                 g)))
           {}
           (get a "var-usages" []))]
    (reduce (fn [g r]
              (if (and (:via r) (:owner r))
                (update g (:via r) (fnil conj #{})
                        {:callee (:owner r)
                         :filename (:file r)
                         :line (:from r) :col 1
                         :end-line (:from r) :end-col 1})
                g))
            g regions)))

(defn- fixpoint
  "Grow `seed` along the graph until it stops growing. `edges` maps a node to
  the nodes it depends on."
  [seed edges]
  (loop [acc seed]
    (let [grown (into acc
                      (for [[node deps] edges
                            :when (and (not (contains? acc node))
                                       (some acc deps))]
                        node))]
      (if (= grown acc) acc (recur grown)))))

(defn propagate
  "Given the call graph and the vars known to taint or reach directly,
  compute the transitive closures and return the vars where both meet."
  [graph {:keys [taints reaches]}]
  (let [edges (into {} (for [[caller calls] graph]
                         [caller (set (map :callee calls))]))
        taints'  (fixpoint (set taints) edges)
        reaches' (fixpoint (set reaches) edges)]
    {:taints taints'
     :reaches reaches'
     :paths (into #{} (filter #(and (contains? taints' %) (contains? reaches' %)))
                  (keys graph))}))

(defn call-sites
  "Where `caller` calls anything in `targets` -- the positions that make up
  the reported flow."
  [graph caller targets]
  (->> (get graph caller)
       (filter #(contains? targets (:callee %)))
       (sort-by (juxt :line :col))))

(defn findings
  "Interprocedural findings: one per var that both taints and reaches, with a
  flow through the call sites that make the path."
  [analysis-text {:keys [taints reaches] :as direct}]
  (let [graph (call-graph analysis-text)
        {:keys [paths] :as closed} (propagate graph direct)]
    (for [caller paths
          :when (not (and (contains? (set taints) caller)
                          (contains? (set reaches) caller)))
          ;; ONE site is enough, and requiring two silently discarded the
          ;; shape this pass exists for. When a var reads the request itself
          ;; and hands the value to a function in another namespace that
          ;; sinks it, the only tracked call site is the call into that
          ;; function -- the source is clojure.core/get-in, which is never a
          ;; project var and so never appears in the closure. Measured: a
          ;; handler and a db namespace, seeds correct, call graph correct,
          ;; and zero findings.
          :let [sites (call-sites graph caller
                                  (into (:taints closed) (:reaches closed)))
                [a b] (take 2 sites)]
          :when a]
      {:rule "interprocedural-taint"
       :caller caller
       :filename (:filename a)
       :line (:line a) :col (:col a)
       :end-line (:end-line a) :end-col (:end-col a)
       ;; `str` rather than `format`: format is JVM-only, and this namespace
       ;; compiles to ClojureScript inside defnet.
       ;; name the ends: the self-run on defnet gave 21 of these and not one
       ;; said WHICH source or sink, so none could be judged from the message
       :source (some-> (:callee a) (#(str (first %) "/" (second %))))
       :sink (some-> (:callee (or b a)) (#(str (first %) "/" (second %))))
       :message (str (first caller) "/" (second caller)
                     " obtains attacker-influenced data via " (some-> (:callee a) second)
                     (when b (str " and passes it toward a sink via " (some-> (:callee b) second))))
       :flow (cond-> [{:line (:line a) :col (:col a)
                       :end-line (:end-line a) :end-col (:end-col a)
                       :message (if b
                                  "attacker-influenced value obtained here"
                                  "attacker-influenced value passed toward a sink here")}]
               b (conj {:line (:line b) :col (:col b)
                        :end-line (:end-line b) :end-col (:end-col b)
                        :message "and passed toward a sink here"}))})))
