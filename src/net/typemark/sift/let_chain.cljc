(ns net.typemark.sift.let-chain
  (:require [net.typemark.sift.portable.shape :as shape]
            [net.typemark.sift.zip :refer [inside-defn? collect pos-of sexpr]]))

(defn findings
  [zloc threshold]
  (vec (keep (fn [loc]
               (when (inside-defn? loc)
                 (let [form (sexpr loc ::no)]
                   (when-let [hit (and (not= ::no form) (shape/let-as-thread form threshold))]
                     (let [[line col] (or (pos-of loc) [nil nil])]
                       (assoc hit :line line :column col))))))
             (collect zloc (fn [z] (let [s (sexpr z ::no)
                                         f (when (seq? s) (first s))]
                                     (and (symbol? f) (nil? (namespace f))
                                          (contains? #{"let" "let*"} (name f)))))))))
