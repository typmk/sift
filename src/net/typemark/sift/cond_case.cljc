(ns net.typemark.sift.cond-case
  (:require [net.typemark.sift.portable.cond-case :as portable]
            [net.typemark.sift.zip :refer [list-op op-name inside-defn? collect pos-of sexpr]]))

(defn findings
  [zloc]
  (vec (keep (fn [loc]
               (when (inside-defn? loc)
                 (let [form (sexpr loc ::no)]
                   (when-let [hit (and (not= ::no form) (portable/check form))]
                     (let [[line col] (or (pos-of loc) [nil nil])]
                       (assoc hit :line line :column col))))))
             (collect zloc (fn [z] (= "cond" (op-name (list-op z))))))))
