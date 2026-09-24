(ns net.typemark.sift.cond-build
  "A value built by rebinding one name, conditionally, over and over:

      (let [m {}
            m (if (:a in) (assoc m :a 1) m)
            m (if (:b in) (assoc m :b 2) m)
            m (if (:c in) (assoc m :c 3) m)]
        m)

  which is `cond->`. Same family as place-as-fold and loop-as-reduce — an
  imperative accumulation no branch count sees, because every branch is
  trivial and the complexity is in the repetition. From the nufuturo-ufcg
  Clojure catalogue, where it is Conditional Build-Up.

  Three rebindings at least. Two is a shadow, and saying so is noise: the
  threshold is where the shape stops reading as one decision. Measured over
  1,131 files, 8 of 12,248 `let` forms, and clojure.core's own `defn`
  rebuilds `fdecl` four times.

  The counterpart is mechanical only when every step is
  `(if <test> (<f> <name> <args…>) <name>)` — that is exactly a cond-> pair.
  A step shaped any other way is reported without one."
  (:require [net.typemark.sift.zip :refer [children peel op-name list-op inside-defn?
                                           collect binder-vec vec-pairs sexpr pos-of]]))

(def rule :cond-as-build-up)

(def instruction
  "Do not rebuild one name through a chain of conditional rebindings. Use cond->: (cond-> init test (f args) test (g args)). Each pair is a test and the step it guards.")

(def ^:private conditional-heads '#{if if-not when when-not})

(defn- self-rebind
  "[name step] when this pair conditionally rebinds NAME using NAME, else nil."
  [[lhs rhs]]
  (when (and (symbol? lhs) (seq? rhs)
             (contains? conditional-heads (first rhs))
             (some #(= lhs %) (tree-seq coll? seq rhs)))
    [lhs rhs]))

(defn- cond-pair
  "`(if test (f name args…) name)` -> [test (f args…)], the cond-> pair, or
  nil when the step is shaped otherwise."
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
