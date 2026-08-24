(ns au.com.heisenbergtech.scan.regex
  "Regular-expression rules.

  Java ships 30 of these and Clojure had none, yet the exposure is identical:
  a Clojure regex literal compiles to the same `java.util.regex.Pattern` with
  the same backtracking engine. A pattern with nested quantifiers turns a
  short attacker-supplied string into seconds of CPU, which is a denial of
  service that needs no privilege and leaves no trace but latency.

  Detection is over the literal text, which is what the scanner has and what
  the engine will compile."
  (:require [clojure.string :as str]
            [au.com.heisenbergtech.scan.tree :as tree]))

(def ^:private brace-nested
  "A brace-bounded repetition inside another: (a{1,9}){1,9}.

  This is the shape MEASURED exponential on JDK 25 -- 38ms at n=20, 3.26s at
  n=32. The JDK memoises its Loop node, which is what `(a+)+` compiles to, so
  the textbook nested-plus shape is LINEAR on a modern JVM. Curly nodes are
  not memoised, and this is what remains.

  The previous version of this rule flagged eleven patterns, of which zero
  were measurably super-linear, and missed this one."
  #"\((?:\?:)?[^()]*\{\d+,\d*\}[^()]*\)\s*\{\d+,\d*\}")

(def ^:private nested-quantifier-unmemoised
  "A quantified group whose body is quantified AND which contains a nested
  group, defeating the JDK's memoisation: ((a+))+."
  #"\((?:\?:)?[^)]*\([^()]*[+*][^()]*\)[^(]*\)\s*[+*]")

(defn- pattern-text
  "The source between the delimiters of a #\"...\" literal."
  [{:keys [text]}]
  (when (and text (str/starts-with? text "#\""))
    (subs text 2 (max 2 (dec (count text))))))

(defn- redos? [p]
  (boolean (or (re-find brace-nested p)
               (re-find nested-quantifier-unmemoised p))))

(def ^:private validating
  "Calls whose result decides whether input is acceptable. `re-find` succeeds
  on a PARTIAL match, so a pattern meant to validate a whole value accepts
  anything containing it -- \"evil.com/good.example\" passes a check written
  against `good\\.example`."
  #{"re-find" "re-seq" "re-matcher"})

(defn- anchored? [p]
  (and (str/starts-with? p "^") (str/ends-with? p "$")))

(def ^:private branch-heads
  #{"if" "when" "when-not" "if-not" "and" "or" "cond" "assert" "when-let" "if-let"})

(defn- deciding?
  "True when this match sits in the test position of a branch."
  [nodes l]
  (some (fn [b]
          (when-let [a (tree/first-argument nodes b)]
            (and (= (:line a) (:line l)) (= (:col a) (:col l)))))
        (tree/lists-headed-by nodes branch-heads)))

(defn findings [nodes]
  (concat
   (for [n nodes
         :when (and (= :regex (:type n)) (not (:commented? n)))
         :let [p (pattern-text n)]
         :when (and p (redos? p))]
     {:rule "redos-vulnerable-regex"
      :line (:line n) :col (:col n) :end-line (:end-line n) :end-col (:end-col n)
      :message (str "nested quantifier in " (:text n)
                    " -- a short crafted input can take exponential time")})

   (for [l (tree/lists-headed-by nodes validating)
         :let [a (tree/first-argument nodes l)]
         :when (and a (= :regex (:type a)))
         :let [p (pattern-text a)]
         :when p
         :let [multiline? (str/includes? p "(?m)")
               anchored (anchored? p)]
         :when (or (not anchored) multiline?)
         ;; It must be DECIDING something. `re-find` for extraction or
         ;; detection is the normal use and is not a weakness -- unqualified,
         ;; this rule raised fourteen findings against this plugin's own
         ;; detection code.
         :when (deciding? nodes l)]
     {:rule "partial-match-validation"
      :line (:line l) :col (:col l) :end-line (:end-line l) :end-col (:end-col l)
      :message (str (:head l) " matches anywhere in the string, so this accepts any value"
                    " CONTAINING a match"
                    (when multiline? " -- (?m) makes ^ and $ line anchors, not string anchors"))})))
