(ns net.typemark.sift.pattern
  (:require [clojure.string :as str]))

(defn var-sym?
  [x]
  (and (symbol? x) (nil? (namespace x))
       (str/starts-with? (name x) "?")
       (not= "?_" (name x))
       (not (str/starts-with? (name x) "?&"))))

(defn rest-sym? [x]
  (and (symbol? x) (str/starts-with? (name x) "?&")))

(defn- ellipsis? [x] (= '... x))
(defn- wild? [x] (= '?_ x))
(defn- group? [x] (and (seq? x) (= '?? (first x))))
(defn- group-members [x] (rest x))

(defn vars-in
  [pat]
  (cond
    (var-sym? pat) [pat]
    (rest-sym? pat) [pat]
    (or (seq? pat) (vector? pat)) (vec (distinct (mapcat vars-in pat)))
    (map? pat) (vec (distinct (mapcat vars-in (concat (keys pat) (vals pat)))))
    :else []))

(declare match*)

(defn- match-seq
  [pats forms binds]
  (cond
    (empty? pats)
    (when (empty? forms) binds)

    (rest-sym? (first pats))
    (when (= 1 (count pats))
      (if (= '?&_ (first pats))
        binds
        (assoc binds (first pats) (vec forms))))

    (and (second pats) (ellipsis? (second pats)))
    (let [p (first pats)
          after (drop 2 pats)
          members (if (group? p) (vec (group-members p)) [p])
          width (count members)
          group-vars (vars-in members)
          match-group (fn [fs]
                        (when (>= (count (take width fs)) width)
                          (match-seq members (take width fs) {})))]
      (loop [fs forms reps []]
        (let [b (match-group fs)]
          (if b
            (recur (drop width fs) (conj reps b))
            (loop [k (count reps)]
              (when (>= k 0)
                (let [taken (take k reps)
                      grouped (into {} (for [v group-vars] [v (mapv #(get % v) taken)]))
                      merged (reduce-kv (fn [m v val]
                                          (when m
                                            (if (contains? m v)
                                              (when (= (get m v) val) m)
                                              (assoc m v val))))
                                        binds grouped)
                      rest-forms (drop (* k width) forms)]
                  (or (when merged (match-seq after rest-forms merged))
                      (recur (dec k))))))))))

    :else
    (when (seq forms)
      (when-let [b (match* (first pats) (first forms) binds)]
        (match-seq (rest pats) (rest forms) b)))))

(defn- match* [pat form binds]
  (cond
    (wild? pat) binds

    (var-sym? pat)
    (if (contains? binds pat)
      (when (= (get binds pat) form) binds)
      (assoc binds pat form))

    (seq? pat)
    (when (seq? form) (match-seq pat form binds))

    (vector? pat)
    (when (vector? form) (match-seq (seq pat) (seq form) binds))

    (map? pat)
    (when (map? form)
      (reduce-kv (fn [b k v]
                   (when b
                     (if (contains? form k)
                       (match* v (get form k) b)
                       (reduced nil))))
                 binds pat))

    :else (when (= pat form) binds)))

(defn match
  ([pat form] (match* pat form {}))
  ([pat form binds] (match* pat form (or binds {}))))

(declare substitute)

(defn- subst-seq [tmpl binds]
  (loop [ts (seq tmpl) out []]
    (cond
      (empty? ts) out

      (and (second ts) (ellipsis? (second ts)))
      (let [t (first ts)
            members (if (group? t) (vec (group-members t)) [t])
            vs (filter #(vector? (get binds %)) (vars-in members))
            n (if (seq vs) (apply max (map #(count (get binds %)) vs)) 0)]
        (recur (drop 2 ts)
               (into out (for [i (range n)
                               m members]
                           (substitute m (reduce (fn [b v] (assoc b v (nth (get binds v) i)))
                                                 binds vs))))))

      (rest-sym? (first ts))
      (recur (rest ts) (into out (get binds (first ts))))

      :else (recur (rest ts) (conj out (substitute (first ts) binds))))))

(defn substitute
  [tmpl binds]
  (cond
    (var-sym? tmpl) (get binds tmpl tmpl)
    (seq? tmpl) (apply list (subst-seq tmpl binds))
    (vector? tmpl) (subst-seq tmpl binds)
    (map? tmpl) (into {} (map (fn [[k v]] [(substitute k binds) (substitute v binds)])) tmpl)
    :else tmpl))
