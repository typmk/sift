(ns net.typemark.sift.loop-fold
  (:require [net.typemark.sift.portable.shape :as shape]
            [net.typemark.sift.zip :refer [list-op op-name inside-defn? collect pos-of sexpr]]))

(defn findings
  [zloc]
  (vec (keep (fn [loc]
               (when (inside-defn? loc)
                 (let [form (sexpr loc ::no)]
                   (when-let [hit (and (not= ::no form) (shape/loop-as-reduce form))]
                     (let [[line col] (or (pos-of loc) [nil nil])]
                       (assoc hit :line line :column col))))))
             (collect zloc (fn [z] (= "loop" (op-name (list-op z))))))))
