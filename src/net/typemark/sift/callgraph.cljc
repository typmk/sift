(ns net.typemark.sift.callgraph
  (:require [net.typemark.sift.json :as json]))

(def ^:private js-max-row
  1000000)

(defn- row-of [m] (or (get m "row") (get m "name-row")))

(def ^:private computed-dispatch
  "<computed>")

(defn- literal-dispatch?
  [d]
  (and (seq d)
       (or (= \" (first d))
           (not (some #{\( \) \{ \} \~ \@ \` \#} d)))))

(defn- arm-name
  [u]
  (when (get u "defmethod")
    (let [d (get u "dispatch-val-str")
          d (if (and d (= \: (first d))) (subs d 1) d)]
      (when (seq d)
        (str (get u "name") "::" (if (literal-dispatch? d) d computed-dispatch))))))

(defn- owner-regions
  [a]
  (let [impls (for [p (get a "protocol-impls" [])]
                {:file (get p "filename")
                 :from (get p "row")
                 :to   (get p "end-row")
                 :owner [(get p "impl-ns")
                         (str (get p "protocol-name") "/" (get p "method-name"))]
                 :via  [(get p "protocol-ns") (get p "method-name")]})
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
  [regions file row]
  (when (and file row)
    (->> regions
         (filter #(and (= file (:file %))
                       (<= (:from %) row)
                       (<= row (:to %))))
         (sort-by :from >)
         first)))

(defn call-graph
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
  [seed edges]
  (loop [acc seed]
    (let [grown (into acc
                      (for [[node deps] edges
                            :when (and (not (contains? acc node))
                                       (some acc deps))]
                        node))]
      (if (= grown acc) acc (recur grown)))))

(defn propagate
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
  [graph caller targets]
  (->> (get graph caller)
       (filter #(contains? targets (:callee %)))
       (sort-by (juxt :line :col))))

(defn findings
  [analysis-text {:keys [taints reaches] :as direct}]
  (let [graph (call-graph analysis-text)
        {:keys [paths] :as closed} (propagate graph direct)]
    (for [caller paths
          :when (not (and (contains? (set taints) caller)
                          (contains? (set reaches) caller)))
          :let [sites (call-sites graph caller
                                  (into (:taints closed) (:reaches closed)))
                [a b] (take 2 sites)]
          :when a]
      {:rule "interprocedural-taint"
       :caller caller
       :filename (:filename a)
       :line (:line a) :col (:col a)
       :end-line (:end-line a) :end-col (:end-col a)
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
