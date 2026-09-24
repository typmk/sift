(ns net.typemark.sift.let-chain
  (:require [net.typemark.sift.zip :refer [inside-defn?
                                           collect binder-vec vec-pairs sexpr pos-of]]))



(def ^:private conditional-heads
  '#{if if-not when when-not cond condp case if-let when-let if-some when-some})

(defn- link
  [prev rhs]
  (when (and (seq? rhs) (symbol? (first rhs))
             (not (contains? conditional-heads (first rhs)))
             (= 1 (count (filter #(= prev %) (tree-seq coll? seq rhs)))))
    (let [args (vec (rest rhs))]
      (cond (= prev (peek args)) :last
            (= prev (first args)) :first
            :else nil))))

(defn- finding [zloc threshold]
  (when-let [v (binder-vec zloc)]
    (let [pairs (mapv (fn [[l r]] [(sexpr l ::no) (sexpr r ::no)]) (vec-pairs v))
          body  (last (sexpr zloc ::no))]
      (when (and (>= (count pairs) threshold)
                 (every? (comp symbol? first) pairs)
                 (= body (first (peek pairs))))
        (let [links (map (fn [[[prev _] [_ rhs]]] (link prev rhs))
                         (partition 2 1 pairs))]
          (when (and (every? some? links) (apply = links))
            (let [dir (first links)
                  [line col] (or (pos-of zloc) [nil nil])
                  drop-arg (fn [_ [_ rhs]]
                             (let [args (vec (rest rhs))]
                               (cons (first rhs)
                                     (if (= dir :last) (pop args) (rest args)))))]
              {:line line :column col
               :symbol (str body)
               :message (str (count pairs) " bindings each feeding the next, then returned; use "
                             (if (= dir :last) "->>" "->"))
               :applicability :machine-applicable
               :fix (concat (list (if (= dir :last) '->> '->)
                                          (second (first pairs)))
                                    (map (fn [[a b]] (drop-arg a b)) (partition 2 1 pairs)))})))))))

(defn findings
  [zloc threshold]
  (vec (keep #(when (inside-defn? %) (finding % threshold))
             (collect zloc (fn [z] (let [s (sexpr z ::no)
                                         f (when (seq? s) (first s))]
                                     (and (symbol? f) (nil? (namespace f))
                                          (contains? #{"let" "let*"} (name f)))))))))
