(ns net.typemark.sift.let-chain
  (:require [net.typemark.sift.zip :refer [inside-defn?
                                           collect binder-vec vec-pairs sexpr pos-of]]))

(def rule :let-as-thread)

(def instruction
  "Do not name every intermediate value of one pipeline. Thread it: ->> when each step takes the value LAST, -> when it takes it first. The names are the shape, not information.")

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

(defn- finding [file zloc threshold]
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
              {:rule rule :file file :line line :column col
               :symbol (str body) :shape rule
               :message (str (count pairs) " bindings each feeding the next, then returned; the counterpart is "
                             (if (= dir :last) "->>" "->"))
               :instruction instruction
               :applicability :machine-applicable
               :counterpart (concat (list (if (= dir :last) '->> '->)
                                          (second (first pairs)))
                                    (map (fn [[a b]] (drop-arg a b)) (partition 2 1 pairs)))})))))))

(defn findings
  ([file zloc] (findings file zloc 3))
  ([file zloc threshold]
   (vec (keep #(when (inside-defn? %) (finding file % threshold))
              (collect zloc (fn [z] (let [s (sexpr z ::no)
                                          f (when (seq? s) (first s))]
                                      (and (symbol? f) (nil? (namespace f))
                                           (contains? #{"let" "let*"} (name f))))))))))
