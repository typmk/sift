(ns net.typemark.sift.let-chain
  "A pipeline written as a staircase:

      (let [step1 (map inc xs)
            step2 (filter even? step1)
            step3 (reduce + step2)]
        step3)

  which is `(->> xs (map inc) (filter even?) (reduce +))`. From the
  nufuturo-ufcg Clojure catalogue, where it is Thread Ignorance.

  Three links at least, each init using ONLY the name above it, and the body
  the last name — anything else and threading would change what runs. Two
  refusals the corpus taught:

    a step whose head is a conditional threads WORSE than it reads, so a
    chain with one is no finding at all;
    `p/let` and any other qualified let is not clojure.core's — promise
    sequencing is not a -> chain, and matching on the bare name alone put
    six of logseq's promise chains in the list.

  Measured over 1,131 files: 13 of 12,248 `let` forms after both refusals,
  18 before.

  The counterpart starts at the FIRST binding's init rather than lifting that
  init's own last argument out of it — `(->> (map inc xs) (filter even?) …)`
  where a person would write `(->> xs (map inc) (filter even?) …)`. The two
  expand to the same form; the second is the nicer of two correct answers and
  is not what this emits."
  (:require [net.typemark.sift.zip :refer [children peel op-name list-op inside-defn?
                                           collect binder-vec vec-pairs sexpr pos-of]]))

(def rule :let-as-thread)

(def instruction
  "Do not name every intermediate value of one pipeline. Thread it: ->> when each step takes the value LAST, -> when it takes it first. The names are the shape, not information.")

(def ^:private conditional-heads
  '#{if if-not when when-not cond condp case if-let when-let if-some when-some})

(defn- link
  "How `rhs` consumes `prev`: :last, :first, or nil when it does not, more
  than once, or through a conditional."
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
                  drop-arg (fn [[prev _] [_ rhs]]
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
              ;; `collect` visits every node, so guard the seq? before first
              (collect zloc (fn [z] (let [s (sexpr z ::no)
                                          f (when (seq? s) (first s))]
                                      (and (symbol? f) (nil? (namespace f))
                                           (contains? #{"let" "let*"} (name f))))))))))
