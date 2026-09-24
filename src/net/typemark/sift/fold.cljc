(ns net.typemark.sift.fold
  (:require [net.typemark.sift.zip :refer [children peel list-op op-name token-name pos-of same-form? binder-vec vec-pairs collect inside-defn?]]
            [clojure.walk :as walk]
            [rewrite-clj.zip :as z]))

(def rule :place-as-fold)

(def ^:private mutator-names
  #{"swap!" "reset!" "swap-vals!" "reset-vals!" "compare-and-set!"})

(defn- core-named?
  [sym n]
  (and (symbol? sym)
       (= n (name sym))
       (let [ns (namespace sym)]
         (or (nil? ns) (= "clojure.core" ns) (= "cljs.core" ns)))))

(def ^:dynamic *resolve*
  nil)

(defn- resolved-head
  [list-zloc]
  (when *resolve*
    (get (:vars *resolve*) (pos-of list-zloc))))

(defn- local-call?
  [list-zloc]
  (when *resolve*
    (contains? (:locals *resolve*) (pos-of list-zloc))))

(defn- core-call?
  [list-zloc n]
  (let [z (peel list-zloc)]
    (if-let [q (resolved-head z)]
      (contains? #{(str "clojure.core/" n) (str "cljs.core/" n)} q)
      (and (not (local-call? z))
           (core-named? (list-op z) n)))))

(def instruction
  "Do not accumulate in an atom. Use reduce (or into / group-by). Every branch, including else and catch, must return the accumulator. Do not swap! or reset!.")

(defn- binds-name? [zloc nm]
  (or (when-let [vz (binder-vec zloc)]
        (some (fn [[lhs _]] (= nm (token-name lhs))) (vec-pairs vz)))
      (when (= "fn" (op-name (list-op zloc)))
        (let [xs (children (peel zloc))
              params (some (fn [x] (when (z/vector? x) x)) xs)]
          (when params
            (some (fn [c] (= nm (token-name c))) (children params)))))
      (when (= "letfn" (op-name (list-op zloc)))
        (when-let [v (z/right (z/down (peel zloc)))]
          (some (fn [c]
                  (when (z/list? (peel c))
                    (= nm (token-name (z/down (peel c))))))
                (children v))))))

(defn- ours?
  [token-zloc let-zloc nm]
  (loop [z (z/up token-zloc)]
    (cond
      (nil? z) false
      (same-form? z let-zloc) true
      (binds-name? z nm) false
      :else (recur (z/up z)))))

(defn- fn-head
  [zloc]
  (when (and zloc (= :fn (z/tag zloc)))
    (when-let [h (first (children zloc))]
      (try (z/sexpr h) (catch #?(:clj Exception :cljs :default) _ nil)))))

(defn- classify [token-zloc]
  (let [parent (z/up token-zloc)
        cs (when parent (children parent))]
    (cond
      (and parent (= :deref (z/tag parent))) :deref
      (and parent
           (z/list? parent)
           (= "deref" (op-name (list-op parent)))
           (same-form? (second cs) token-zloc))
      :deref
      (and parent
           (z/list? parent)
           (contains? mutator-names (some-> (list-op parent) name))
           (core-call? parent (name (list-op parent)))
           (same-form? (second cs) token-zloc))
      :mutate
      (and parent
           (= :fn (z/tag parent))
           (contains? mutator-names (op-name (fn-head parent)))
           (core-named? (fn-head parent) (op-name (fn-head parent)))
           (same-form? (second cs) token-zloc))
      :mutate
      (and parent (z/list? parent) (= "recur" (op-name (list-op parent))))
      :carry
      :else :escape)))

(defn- let-init?
  [zloc]
  (let [parent (z/up zloc)
        let-z (when parent (z/up parent))]
    (and parent (z/vector? parent) let-z
         (contains? #{"let" "let*"} (op-name (list-op let-z)))
         (->> (children parent)
              (take-while #(not (same-form? % zloc)))
              count
              odd?))))

(defn- named-fn?
  [zloc]
  (let [xs (children (peel zloc))]
    (and (= "fn" (op-name (list-op zloc)))
         (> (count xs) 2)
         (#{:token} (z/tag (second xs)))
         (symbol? (try (z/sexpr (second xs)) (catch #?(:clj Exception :cljs :default) _ nil))))))

(defn- iife?
  [fn-zloc]
  (let [parent (z/up fn-zloc)]
    (and parent (z/list? parent)
         (same-form? (first (children parent)) fn-zloc))))

(defn- fn-node? [zloc]
  (or (= :fn (z/tag zloc))
      (contains? #{"fn" "fn*" "letfn"} (op-name (list-op zloc)))))

(def ^:private seq-consumers
  #{"run!" "map" "mapv" "keep" "filter" "remove" "reduce" "into" "for" "doseq"})

(defn- fn-escapes?
  [fn-zloc]
  (let [parent (z/up fn-zloc)]
    (not (and parent
              (z/list? parent)
              (contains? seq-consumers (op-name (list-op parent)))
              (not (same-form? (first (children parent)) fn-zloc))))))

(defn- decline-fn?
  [mutate-zloc let-zloc]
  (loop [z mutate-zloc]
    (cond
      (nil? z) false
      (same-form? z let-zloc) false
      (fn-node? z)
      (or (= "letfn" (op-name (list-op z)))
          (let-init? z)
          (and (named-fn? z) (iife? z))
          (fn-escapes? z)
          (recur (z/up z)))
      :else (recur (z/up z)))))

(defn- allowed? [sym-zloc init-zloc]
  (let [m (fn [z]
            (try (:places/allow (meta (z/sexpr z)))
                 (catch #?(:clj Exception :cljs :default) _ nil)))]
    (boolean (or (m sym-zloc) (m init-zloc)))))

(defn- atom-call? [zloc]
  (and (z/list? (peel zloc)) (core-call? zloc "atom")))

(defn- atom-init-val [zloc]
  (when-let [init (z/right (z/down (peel zloc)))]
    (try (z/sexpr init) (catch #?(:clj Exception :cljs :default) _ nil))))

(defn- rewrite-ops [form acc-sym]
  (walk/prewalk
   (fn [x]
     (if (seq? x)
       (let [op (first x)
             n (when (symbol? op) (name op))]
         (cond
           (and (#{"swap!" "swap-vals!"} n)
                (= acc-sym (second x))
                (nth x 2 nil))
           (cons (nth x 2) (cons acc-sym (drop 3 x)))
           (and (#{"reset!" "reset-vals!"} n)
                (= acc-sym (second x)))
           (nth x 2)
           (and (= "deref" n) (= acc-sym (second x)))
           acc-sym
           :else x))
       x))
   form))

(defn- doseq-bodies [doseq-zloc]
  (when-let [vz (binder-vec doseq-zloc)]
    (loop [z (z/right vz) acc []]
      (if z (recur (z/right z) (conj acc z)) acc))))

(defn- lone-mutator? [body-z acc-sym]
  (let [z (peel body-z)]
    (and z (z/list? z)
         (contains? mutator-names (op-name (list-op z)))
         (let [arg1 (second (children z))]
           (= (name acc-sym) (token-name arg1))))))

(defn- simple-doseq [doseq-zloc acc-sym]
  (when (= "doseq" (op-name (list-op doseq-zloc)))
    (when-let [vz (binder-vec doseq-zloc)]
      (let [pairs (map (fn [[l r]]
                         [(try (z/sexpr (peel l)) (catch #?(:clj Exception :cljs :default) _ ::no))
                          (try (z/sexpr r) (catch #?(:clj Exception :cljs :default) _ ::no))])
                       (vec-pairs vz))
            bodies (doseq-bodies doseq-zloc)]
        (when (and (= 1 (count pairs))
                   (not (keyword? (ffirst pairs)))
                   (not-any? #{::no} (first pairs))
                   (= 1 (count bodies))
                   (lone-mutator? (first bodies) acc-sym))
          (let [bind (ffirst pairs)
                coll (second (first pairs))
                body (try (z/sexpr (first bodies)) (catch #?(:clj Exception :cljs :default) _ ::no))]
            (when (not= ::no body)
              {:bind bind
               :coll coll
               :body (rewrite-ops body acc-sym)})))))))

(defn- counterpart [let-zloc acc-sym init-val mutates]
  (let [doseqs (collect let-zloc
                        (fn [z] (= "doseq" (op-name (list-op z)))))
        host (some (fn [x] (when ((fn [d]
                              (some (fn [m]
                                      (loop [z m]
                                        (cond
                                          (nil? z) false
                                          (same-form? z d) true
                                          :else (recur (z/up z)))))
                                    mutates)) x) x)) doseqs)]
    (when (and host (= 1 (count doseqs)))
      (when-let [{:keys [bind coll body]} (simple-doseq host acc-sym)]
        {:form (list 'reduce
                     (list 'fn [acc-sym bind] body)
                     init-val
                     coll)
         :applicability :machine-applicable}))))

(defn- finding [file let-zloc sym-zloc init-zloc usages]
  (let [nm (token-name sym-zloc)
        acc-sym (try (z/sexpr (peel sym-zloc)) (catch #?(:clj Exception :cljs :default) _ (symbol nm)))
        mutates (keep (fn [{:keys [zloc kind]}]
                        (when (= :mutate kind)
                          (z/up zloc)))
                      usages)
        [line col] (or (pos-of (peel init-zloc)) (pos-of (peel sym-zloc)) [nil nil])
        cp (counterpart let-zloc acc-sym (atom-init-val init-zloc) mutates)]
    (cond-> {:rule rule
             :file file
             :line line
             :column col
             :symbol acc-sym
             :shape :atom-as-fold
             :message "place used as a fold; the counterpart is reduce"
             :instruction instruction
             :applicability (or (:applicability cp) :unspecified)}
      (:form cp) (assoc :counterpart (:form cp)))))

(defn- verdict [file let-zloc sym-zloc init-zloc]
  (when-not (allowed? sym-zloc init-zloc)
    (let [nm (token-name sym-zloc)
          bind* (peel sym-zloc)
          tokens (collect let-zloc
                          (fn [z] (and (= nm (token-name z))
                                       (not (same-form? z bind*))
                                       (ours? z let-zloc nm))))
          usages (map (fn [t] {:zloc t :kind (classify t)}) tokens)
          kinds (set (map :kind usages))
          mutate-z (keep (fn [{:keys [zloc kind]}]
                           (when (= :mutate kind) (z/up zloc)))
                         usages)]
      (when (seq mutate-z)
        (cond
          (contains? kinds :escape) nil
          (some #(decline-fn? % let-zloc) mutate-z) nil
          :else (finding file let-zloc sym-zloc init-zloc usages))))))

(defn- atom-bindings [let-zloc]
  (when-let [vz (binder-vec let-zloc)]
    (for [[lhs rhs] (vec-pairs vz)
          :when (and (token-name lhs) (atom-call? rhs))]
      [lhs rhs])))

(defn findings
  [file zloc]
  (vec
   (mapcat
    (fn [let-z]
      (when (inside-defn? let-z)
        (keep (fn [[sym init]]
                (verdict file let-z sym init))
              (atom-bindings let-z))))
    (collect zloc (fn [z] (contains? #{"let" "let*" "loop"} (op-name (list-op z))))))))
