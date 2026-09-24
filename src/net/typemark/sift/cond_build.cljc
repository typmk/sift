(ns net.typemark.sift.cond-build
  (:require [net.typemark.sift.zip :refer [op-name list-op inside-defn?
                                           collect binder-vec vec-pairs sexpr pos-of]]))

(def rule :cond-as-build-up)

(def instruction
  "Do not rebuild one name through a chain of conditional rebindings. Use cond->: (cond-> init test (f args) test (g args)). Each pair is a test and the step it guards.")

(def ^:private conditional-heads '#{if if-not when when-not})

(defn- self-rebind
  [[lhs rhs]]
  (when (and (symbol? lhs) (seq? rhs)
             (contains? conditional-heads (first rhs))
             (some #(= lhs %) (tree-seq coll? seq rhs)))
    [lhs rhs]))

(defn- cond-pair
  [nm [h test then else]]
  (when (and (= 'if h) (= nm else) (seq? then) (= nm (second then)))
    [test (cons (first then) (drop 2 then))]))

(defn- finding [file zloc threshold]
  (when-let [v (binder-vec zloc)]
    (let [pairs (map (fn [[l r]] [(sexpr l ::no) (sexpr r ::no)]) (vec-pairs v))
          steps (keep self-rebind pairs)
          by-name (group-by first steps)]
      (when-let [[nm ss] (first (filter #(>= (count (val %)) threshold) by-name))]
        (let [cps (map #(cond-pair nm (second %)) ss)
              mechanical? (every? some? cps)
              init (second (first (filter #(= nm (first %)) pairs)))
              [line col] (or (pos-of zloc) [nil nil])]
          (cond-> {:rule rule :file file :line line :column col
                   :symbol (str nm) :shape rule
                   :message (str nm " is conditionally rebound " (count ss)
                                 " times in one let; the counterpart is cond->")
                   :instruction instruction
                   :applicability (if mechanical? :machine-applicable :unspecified)}
            mechanical? (assoc :counterpart
                               (concat (list 'cond-> init) (mapcat identity cps)))))))))

(defn findings
  ([file zloc] (findings file zloc 3))
  ([file zloc threshold]
   (vec (keep #(when (inside-defn? %) (finding file % threshold))
              (collect zloc (fn [z] (contains? #{"let" "let*"} (op-name (list-op z)))))))))
