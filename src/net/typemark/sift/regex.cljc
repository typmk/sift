(ns net.typemark.sift.regex
  (:require [clojure.string :as str]
            [net.typemark.sift.tree :as tree]))

(def ^:private brace-nested
  #"\((?:\?:)?[^()]*\{\d+,\d*\}[^()]*\)\s*\{\d+,\d*\}")

(def ^:private nested-quantifier-unmemoised
  #"\((?:\?:)?[^)]*\([^()]*[+*][^()]*\)[^(]*\)\s*[+*]")

(defn- pattern-text
  [{:keys [text]}]
  (when (and text (str/starts-with? text "#\""))
    (subs text 2 (max 2 (dec (count text))))))

(defn- redos? [p]
  (boolean (or (re-find brace-nested p)
               (re-find nested-quantifier-unmemoised p))))

(def ^:private validating
  #{"re-find" "re-seq" "re-matcher"})

(defn- anchored? [p]
  (and (str/starts-with? p "^") (str/ends-with? p "$")))

(def ^:private branch-heads
  #{"if" "when" "when-not" "if-not" "and" "or" "cond" "assert" "when-let" "if-let"})

(defn- deciding?
  [nodes l]
  (some (fn [b]
          (when-let [a (tree/first-argument nodes b)]
            (and (= (:line a) (:line l)) (= (:col a) (:col l)))))
        (tree/lists-headed-by nodes branch-heads)))

(defn redos-vulnerable-regex
  [nodes]
  (for [n nodes
        :when (and (= :regex (:type n)) (not (:commented? n)))
        :let [p (pattern-text n)]
        :when (and p (redos? p))]
    (tree/hit n (str "nested quantifier in " (:text n)
                     " -- a short crafted input can take exponential time"))))

(defn partial-match-validation
  [nodes]
  (for [l (tree/lists-headed-by nodes validating)
        :let [a (tree/first-argument nodes l)]
        :when (and a (= :regex (:type a)))
        :let [p (pattern-text a)]
        :when p
        :let [multiline? (str/includes? p "(?m)")]
        :when (or (not (anchored? p)) multiline?)
        :when (deciding? nodes l)]
    (tree/hit l (str (:head l) " matches anywhere in the string, so this accepts any value"
                     " CONTAINING a match"
                     (when multiline? " -- (?m) makes ^ and $ line anchors, not string anchors")))))
