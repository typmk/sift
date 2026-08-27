(ns com.typemark.sift.loop-fold
  "Fourth raise: a two-binding loop that walks a seq and threads an
  accumulator is a `reduce`.

     (loop [xs coll acc init]
       (if (seq xs)
         (recur (rest xs) (f acc (first xs)))
         acc))

  The seq binding is whichever one the test names — `(seq xs)`, `(empty? xs)`,
  `(if-not (seq xs) …)` — the other is the accumulator, and binding order
  does not matter. The recur must step the seq by `rest`/`next` and the
  exhausted branch must return the accumulator itself; a loop that returns
  `(count acc)` or steps two at a time or grows the seq is something else.

  Mechanical when the rewritten step mentions the accumulator and no longer
  mentions the seq — `(first xs)` became the element — otherwise a `maybe`
  with the form still attached. `loop-as-map` runs first and owns the
  `[]`/`conj` special case; this rule skips a loop that one already named."
  (:require [rewrite-clj.zip :as z]))

(def rule :loop-as-reduce)

(def instruction
  "Do not walk a seq with loop/recur to thread an accumulator. Use (reduce (fn [acc x] …) init coll). Keep the step expression; drop the seq binding and the exhaustion test.")

(defn- children [zloc]
  (loop [z (z/down zloc) acc []]
    (if z (recur (z/right z) (conj acc z)) acc)))

(defn- peel [zloc]
  (loop [z zloc]
    (if (and z (= :meta (z/tag z)))
      (recur (last (children z)))
      z)))

(defn- list-op [zloc]
  (let [z (peel zloc)]
    (when (and z (z/list? z))
      (when-let [h (z/down z)]
        (try (z/sexpr h) (catch #?(:clj Exception :cljs :default) _ nil))))))

(defn- op-name [sym] (when (symbol? sym) (name sym)))

(defn- call? [form n]
  (and (seq? form) (symbol? (first form)) (= n (name (first form)))))

(defn- seq-of? [form xs] (and (call? form "seq") (= xs (second form))))
(defn- empty-of? [form xs] (and (call? form "empty?") (= xs (second form))))
(defn- step-of? [form xs]
  (and (or (call? form "rest") (call? form "next")) (= xs (second form))))
(defn- first-of? [form xs] (and (call? form "first") (= xs (second form))))

(defn- inside-defn? [zloc]
  (loop [z (z/up zloc)]
    (cond
      (nil? z) false
      (contains? #{"defn" "defn-" "fn" "fn*" "defmacro" "defmethod"} (op-name (list-op z))) true
      :else (recur (z/up z)))))

(defn- collect [zloc pred]
  (let [acc (volatile! [])]
    (letfn [(w [z]
              (when (pred z) (vswap! acc conj z))
              (when-not (or (= :uneval (z/tag z))
                            (= :quote (z/tag z))
                            (contains? #{"comment" "quote"} (op-name (list-op z))))
                (doseq [c (children z)] (w c))))]
      (w zloc)
      @acc)))

(defn- two-binds [zloc]
  (let [vz (when-let [d (z/down (peel zloc))]
             (let [v (z/right d)] (when (z/vector? v) v)))]
    (when vz
      (let [pairs (loop [z (z/down vz) acc []]
                    (if (nil? z)
                      acc
                      (if-let [rhs (z/right z)]
                        (recur (z/right rhs)
                               (conj acc [(try (z/sexpr z) (catch #?(:clj Exception :cljs :default) _ ::no))
                                          (try (z/sexpr rhs) (catch #?(:clj Exception :cljs :default) _ ::no))]))
                        acc)))]
        (when (and (= 2 (count pairs))
                   (every? #(and (symbol? (first %)) (not= ::no (second %))) pairs))
          pairs)))))

(defn- loop-body [zloc]
  (when-let [d (z/down (peel zloc))]
    (when-let [vz (z/right d)]
      (when-let [body (z/right vz)]
        (when (nil? (z/right body))
          (try (z/sexpr body) (catch #?(:clj Exception :cljs :default) _ nil)))))))

(defn- split-if
  "`(if T A B)` -> {:xs sym :walk A :done B} when T names the seq via
  seq/empty?/not/if-not, with `done` the exhausted branch. nil otherwise."
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
  "`(recur R1 R2)` or `(let [el (first xs)] (recur R1 R2))` -> {:expr :el}
  where the seq position is `(rest xs)` and the other is the new acc."
  [form xs acc xs-first?]
  (let [[el inner] (if (and (call? form "let") (vector? (second form)) (= 2 (count (second form)))
                            (= 3 (count form)) (symbol? (first (second form)))
                            (first-of? (second (second form)) xs))
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

(defn- rewrite [expr xs el x]
  (let [w (fn w [e]
            (cond
              (first-of? e xs) x
              (and el (= e el)) x
              (seq? e) (apply list (map w e))
              (vector? e) (mapv w e)
              (map? e) (into {} (map (fn [[k v]] [(w k) (w v)])) e)
              (set? e) (into #{} (map w) e)
              :else e))]
    (w expr)))

(defn- finding [file zloc binds]
  (let [syms (map first binds)
        body (loop-body zloc)]
    (when-let [{:keys [xs walk done]} (split-if body syms)]
      (let [acc (first (remove #{xs} syms))
            xs-first? (= xs (ffirst binds))
            coll (second (first (filter #(= xs (first %)) binds)))
            init (second (first (filter #(= acc (first %)) binds)))]
        (when (and acc (= done acc))
          (when-let [{:keys [expr el]} (step walk xs acc xs-first?)]
            (let [x (or el 'x)
                  body' (rewrite expr xs el x)
                  mechanical? (and (mentions? body' acc) (not (mentions? body' xs)))
                  [line col] (try (z/position zloc) (catch #?(:clj Exception :cljs :default) _ [nil nil]))]
              {:rule rule
               :file file
               :line line
               :column col
               :symbol acc
               :shape :loop-as-reduce
               :message (str "loop threads " acc " over " xs "; the counterpart is reduce")
               :instruction instruction
               :applicability (if mechanical? :mechanical :maybe)
               :counterpart (list 'reduce (list 'fn [acc x] body') init coll)})))))))

(defn findings
  "`taken` is the set of [line col] positions another loop rule already
  reported, so one loop gets one finding."
  ([file zloc] (findings file zloc #{}))
  ([file zloc taken]
   (vec (keep (fn [lz]
                (when (and (inside-defn? lz)
                           (not (contains? taken (try (z/position lz) (catch #?(:clj Exception :cljs :default) _ nil)))))
                  (when-let [b (two-binds lz)]
                    (finding file lz b))))
              (collect zloc (fn [z] (= "loop" (op-name (list-op z)))))))))
