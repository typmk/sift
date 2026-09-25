(ns net.typemark.sift.complexity
  (:require [net.typemark.sift.portable.complexity :as engine]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]))

#?(:clj (set! *warn-on-reflection* true))

(def ops
  {:tag n/tag
   :text n/string
   :inner? n/inner?
   :children n/children
   :fn-params (fn [node] (count (distinct (re-seq #"%\d*&?" (n/string node)))))})

(defn report-of
  [root path]
  (engine/report-root ops root (engine/features-for path)))

(defn report
  ([source] (report source nil))
  ([source path]
   (try (report-of (p/parse-string-all source) path)
        (catch #?(:clj Exception :cljs :default) e {:ok? false :error (ex-message e)}))))

(def flatten-units engine/flatten-units)
