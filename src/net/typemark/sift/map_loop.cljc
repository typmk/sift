(ns net.typemark.sift.map-loop
  "Second raise: a two-binding loop that is (into [] (map f) xs).

   Bindings: a seq and an out starting at []. Recur is (rest/next xs) plus
   (conj out expr). C3 / BFS / retry do not match that."
  (:require [net.typemark.sift.zip :refer [list-op op-name call? inside-defn? collect two-binds single-body pos-of]]))

(def rule :loop-as-map)

(def instruction
  "Do not walk a seq with loop/recur and conj onto an out vector. Use (into [] (map f) xs) or (into [] (comp (filter p) (map f)) xs). Keep the element transform; drop the out binding.")

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

(defn- counterpart [src expr xs el pred]
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

(defn- finding [file zloc binds]
  (when-let [{:keys [xs src out xs-first?]} (classify-binds binds)]
    (when-let [{:keys [expr el pred]} (match-if (single-body zloc) xs out xs-first?)]
      (let [[line col] (or (pos-of zloc) [nil nil])
            cp (counterpart src expr xs el pred)]
        (cond-> {:rule rule
                 :file file
                 :line line
                 :column col
                 :symbol xs
                 :shape :loop-as-map
                 :message "loop is a map; the counterpart is into/map"
                 :instruction instruction
                 :applicability (:applicability cp)}
          (:form cp) (assoc :counterpart (:form cp)))))))

(defn findings
  [file zloc]
  (vec
   (keep (fn [lz]
           (when (inside-defn? lz)
             (when-let [b (two-binds lz)]
               (finding file lz b))))
         (collect zloc (fn [z] (= "loop" (op-name (list-op z))))))))
