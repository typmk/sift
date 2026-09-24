(ns docs
  "Utilities.")

(defn parse-config
  "Parses the config."
  [path opts]
  (slurp path))

(defn total-of
  "This function is used to basically compute the total of the given xs. It simply reduces."
  [xs]
  (reduce + xs))

(defn undocumented [x] x)

(defn tidy
  "TODO write this"
  [x] x)

(defn well-documented
  "Sum of `xs` after `f` is applied to each; nil-safe on an empty `xs`."
  [f xs]
  (reduce + 0 (map f xs)))
