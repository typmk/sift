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

(defn call-graph
  "analysis JSON -> {[ns var] #{[callee-ns callee-var]}} plus the position of
  each call site."
  [analysis-text]
  (let [a (get (json/read-str analysis-text) "analysis")]
    (reduce
     (fn [g u]
       (let [from (get u "from-var")
             fns' (get u "from")
             to   (get u "to")
             nm   (get u "name")]
         (if (and from fns' to nm)
           (update g [fns' from] (fnil conj #{})
                   {:callee [to nm]
                    :filename (get u "filename")
                    :line (get u "name-row") :col (get u "name-col")
                    :end-line (get u "name-end-row") :end-col (get u "name-end-col")})
           g)))
     {}
     (get a "var-usages" []))))

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
