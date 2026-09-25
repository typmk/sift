(ns net.typemark.sift.portable.shape)

(defn- call?
  [form n]
  (and (seq? form) (symbol? (first form)) (= n (name (first form)))))

(defn- bind-pairs
  [form]
  (let [v (when (seq? form) (second form))]
    (when (vector? v) (mapv vec (partition 2 v)))))

(defn- two-binds
  [form]
  (let [pairs (bind-pairs form)]
    (when (and (= 2 (count pairs)) (every? (comp symbol? first) pairs))
      pairs)))

(defn- single-body
  [form]
  (when (and (seq? form) (= 3 (count form))) (nth form 2)))

(defn- rest-of? [form xs]
  (and (or (call? form "rest") (call? form "next"))
       (= xs (second form))))

(defn- first-of? [form xs]
  (and (call? form "first") (= xs (second form))))

(defn- seq-test? [form xs]
  (and (call? form "seq") (= xs (second form))))

(defn- empty-test? [form xs]
  (or (and (call? form "empty?") (= xs (second form)))
      (and (call? form "not") (seq-test? (second form) xs))))

(defn- match-out [out-arg out]
  (cond
    (and (call? out-arg "conj") (= out (second out-arg)) (= 3 (count out-arg)))
    {:expr (nth out-arg 2)}
    (and (call? out-arg "if") (= 4 (count out-arg))
         (= out (nth out-arg 3))
         (call? (nth out-arg 2) "conj")
         (= out (second (nth out-arg 2)))
         (= 3 (count (nth out-arg 2))))
    {:pred (nth out-arg 1)
     :expr (nth (nth out-arg 2) 2)}))

(defn- match-recur [form xs out xs-first?]
  (when (and (call? form "recur") (= 3 (count form)))
    (let [a (nth form 1)
          b (nth form 2)
          [xs-arg out-arg] (if xs-first? [a b] [b a])]
      (when (rest-of? xs-arg xs)
        (match-out out-arg out)))))

(defn- match-then [form xs out xs-first?]
  (or (match-recur form xs out xs-first?)
      (when (and (call? form "let")
                 (vector? (second form))
                 (= 2 (count (second form)))
                 (= 3 (count form)))
        (let [x (first (second form))
              init (second (second form))
              body (nth form 2)]
          (when (and (symbol? x) (first-of? init xs))
            (when-let [m (match-recur body xs out xs-first?)]
              (assoc m :el x)))))))

(defn- match-if [form xs out xs-first?]
  (when (and (call? form "if") (= 4 (count form)))
    (let [t (nth form 1)
          then (nth form 2)
          else (nth form 3)]
      (cond
        (and (seq-test? t xs) (= else out))
        (match-then then xs out xs-first?)
        (and (empty-test? t xs) (= then out))
        (match-then else xs out xs-first?)))))

(defn- rewrite-el [expr xs el]
  (let [x (or el 'x)
        walk (fn walk [e]
               (cond
                 (first-of? e xs) x
                 (and el (= e el)) x
                 (seq? e) (apply list (map walk e))
                 (vector? e) (mapv walk e)
                 :else e))]
    (walk expr)))

(defn- fix-of [src expr xs el pred]
  (let [body (rewrite-el expr xs el)
        pred* (when pred (rewrite-el pred xs el))
        map-only? (and (seq? body) (symbol? (first body)) (= 2 (count body)) (= 'x (second body)))
        pred-sym? (and (seq? pred*) (symbol? (first pred*)) (= 2 (count pred*)) (= 'x (second pred*)))]
    (cond
      (and pred pred-sym? map-only?)
      {:form (list 'into [] (list 'comp (list 'filter (first pred*)) (list 'map (first body))) src)
       :applicability :machine-applicable}
      pred
      {:form (list 'into [] (list 'keep (list 'fn ['x] (list 'when pred* body))) src)
       :applicability :maybe-incorrect}
      map-only?
      {:form (list 'into [] (list 'map (first body)) src)
       :applicability :machine-applicable}
      :else
      {:form (list 'into [] (list 'map (list 'fn ['x] body)) src)
       :applicability :maybe-incorrect})))

(defn- classify-binds [pairs]
  (let [[[a ai] [b bi]] pairs]
    (cond
      (and (symbol? a) (symbol? b) (= [] bi))
      {:xs a :src ai :out b :xs-first? true}
      (and (symbol? a) (symbol? b) (= [] ai))
      {:xs b :src bi :out a :xs-first? false})))

(defn loop-as-map
  [form]
  (when-let [binds (two-binds form)]
    (when-let [{:keys [xs src out xs-first?]} (classify-binds binds)]
      (when-let [{:keys [expr el pred]} (match-if (single-body form) xs out xs-first?)]
        (let [cp (fix-of src expr xs el pred)]
          (cond-> {:symbol xs
                   :message "loop is a map; use into with map"
                   :applicability (:applicability cp)}
            (:form cp) (assoc :fix (:form cp))))))))

(defn- seq-of? [form xs] (and (call? form "seq") (= xs (second form))))
(defn- empty-of? [form xs] (and (call? form "empty?") (= xs (second form))))
(defn- step-of? [form xs]
  (and (or (call? form "rest") (call? form "next")) (= xs (second form))))
(defn- lf-first-of? [form xs] (and (call? form "first") (= xs (second form))))

(defn- split-if
  [form syms]
  (when (and (seq? form) (= 4 (count form)) (contains? #{'if 'if-not} (first form)))
    (let [[op t a b] form
          [a b] (if (= op 'if-not) [b a] [a b])
          t' (if (call? t "not") (second t) t)
          negated? (call? t "not")
          [a b] (if negated? [b a] [a b])
          xs (some (fn [s] (when (or (seq-of? t' s) (empty-of? t' s)) s)) syms)]
      (when xs
        (if (seq-of? t' xs)
          {:xs xs :walk a :done b}
          {:xs xs :walk b :done a})))))

(defn- step
  [form xs acc xs-first?]
  (let [[el inner] (if (and (call? form "let") (vector? (second form)) (= 2 (count (second form)))
                            (= 3 (count form)) (symbol? (first (second form)))
                            (lf-first-of? (second (second form)) xs))
                     [(first (second form)) (nth form 2)]
                     [nil form])]
    (when (and (call? inner "recur") (= 3 (count inner)))
      (let [[_ r1 r2] inner
            [xs-arg acc-arg] (if xs-first? [r1 r2] [r2 r1])]
        (when (and (step-of? xs-arg xs) (not= acc-arg acc))
          {:expr acc-arg :el el})))))

(defn- mentions? [form sym]
  (cond (= form sym) true
        (or (seq? form) (vector? form) (map? form) (set? form)) (some #(mentions? % sym) form)
        :else false))

(defn- lf-rewrite [expr xs el x]
  (let [w (fn w [e]
            (cond
              (lf-first-of? e xs) x
              (and el (= e el)) x
              (seq? e) (apply list (map w e))
              (vector? e) (mapv w e)
              (map? e) (into {} (map (fn [[k v]] [(w k) (w v)])) e)
              (set? e) (into #{} (map w) e)
              :else e))]
    (w expr)))

(defn loop-as-reduce
  [form]
  (when-let [binds (two-binds form)]
    (when-not (loop-as-map form)
      (let [syms (map first binds)
            body (single-body form)]
        (when-let [{:keys [xs walk done]} (split-if body syms)]
          (let [acc (first (remove #{xs} syms))
                xs-first? (= xs (ffirst binds))
                coll (second (some (fn [x] (when (= xs (first x)) x)) binds))
                init (second (some (fn [x] (when (= acc (first x)) x)) binds))]
            (when (and acc (= done acc))
              (when-let [{:keys [expr el]} (step walk xs acc xs-first?)]
                (let [x (or el 'x)
                      body' (lf-rewrite expr xs el x)
                      mechanical? (and (mentions? body' acc) (not (mentions? body' xs)))]
                  {:symbol acc
                   :message (str "loop threads " acc " over " xs "; use reduce")
                   :applicability (if mechanical? :machine-applicable :has-placeholders)
                   :fix (list 'reduce (list 'fn [acc x] body') init coll)})))))))))

(def ^:private cb-conditional-heads '#{if if-not when when-not})

(defn- self-rebind
  [[lhs rhs]]
  (when (and (symbol? lhs) (seq? rhs)
             (contains? cb-conditional-heads (first rhs))
             (some #(= lhs %) (tree-seq coll? seq rhs)))
    [lhs rhs]))

(defn- cond-pair
  [nm [h test then else]]
  (when (and (= 'if h) (= nm else) (seq? then) (= nm (second then)))
    [test (cons (first then) (drop 2 then))]))

(defn cond-as-build-up
  [form threshold]
  (when-let [pairs (bind-pairs form)]
    (let [steps (keep self-rebind pairs)
          by-name (group-by first steps)]
      (when-let [[nm ss] (first (filter #(>= (count (val %)) threshold) by-name))]
        (let [cps (map #(cond-pair nm (second %)) ss)
              mechanical? (every? some? cps)
              init (second (first (filter #(= nm (first %)) pairs)))]
          (cond-> {:symbol (str nm)
                   :message (str nm " is conditionally rebound " (count ss) " times in one let; use cond->")
                   :applicability (if mechanical? :machine-applicable :unspecified)}
            mechanical? (assoc :fix (concat (list 'cond-> init) (mapcat identity cps)))))))))

(def ^:private lc-conditional-heads
  '#{if if-not when when-not cond condp case if-let when-let if-some when-some})

(defn- link
  [prev rhs]
  (when (and (seq? rhs) (symbol? (first rhs))
             (not (contains? lc-conditional-heads (first rhs)))
             (= 1 (count (filter #(= prev %) (tree-seq coll? seq rhs)))))
    (let [args (vec (rest rhs))]
      (cond (= prev (peek args)) :last
            (= prev (first args)) :first
            :else nil))))

(defn let-as-thread
  [form threshold]
  (when-let [pairs (bind-pairs form)]
    (let [body (last form)]
      (when (and (>= (count pairs) threshold)
                 (every? (comp symbol? first) pairs)
                 (= body (first (peek pairs))))
        (let [links (map (fn [[[prev _] [_ rhs]]] (link prev rhs))
                         (partition 2 1 pairs))]
          (when (and (every? some? links) (apply = links))
            (let [dir (first links)
                  drop-arg (fn [_ [_ rhs]]
                             (let [args (vec (rest rhs))]
                               (cons (first rhs)
                                     (if (= dir :last) (pop args) (rest args)))))]
              {:symbol (str body)
               :message (str (count pairs) " bindings each feeding the next, then returned; use "
                             (if (= dir :last) "->>" "->"))
               :applicability :machine-applicable
               :fix (concat (list (if (= dir :last) '->> '->) (second (first pairs)))
                            (map (fn [[a b]] (drop-arg a b)) (partition 2 1 pairs)))})))))))
