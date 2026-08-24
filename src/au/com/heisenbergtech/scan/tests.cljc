(ns au.com.heisenbergtech.scan.tests
  "Rules about the tests themselves.

  Java ships 48 of these; Clojure had one. A test that asserts nothing still
  runs, still passes, and still counts toward coverage -- so it raises the
  number everyone watches while checking nothing. On a codebase with 1,429
  `deftest` forms that is a lot of surface where the signal could be hollow
  and nothing would say so.

  Scoped to test files by the caller; these shapes are legitimate elsewhere."
  (:require [au.com.heisenbergtech.scan.tree :as tree]))

(def ^:private assertions
  "`prop/for-all` is how a defspec asserts. Omitting it reported all fifteen
  of this project's own property tests as asserting nothing."
  #{"is" "are" "expect" "check" "thrown?" "thrown-with-msg?"
    "prop/for-all" "for-all" "prop/for-all*" "checking"})

(defn- assertion-helpers
  "Functions defined in THIS file whose body contains an assertion.

  A suite that factors its assertions into a helper is not a suite that
  asserts nothing, but the rule as first written could not tell the
  difference: it looked for an assertion lexically inside the `deftest` and
  stopped there. Dogfooded against this plugin's own rules-detection suite --
  which asserts through a `fires` helper -- it raised nineteen findings, every
  one of them wrong.

  Whole-file only, and deliberately so. A helper imported from elsewhere is
  invisible here, so this under-reports rather than accusing wrongly; that is
  the correct direction for a rule about test quality, which gets switched off
  the first time it cries wolf."
  [nodes]
  (into #{}
        (for [d (tree/lists-headed-by nodes #{"defn" "defn-"})
              :when (some #(and (= :list (:tag %)) (contains? assertions (:head %)))
                          (tree/children-of nodes d))
              :let [nm (first (tree/arguments nodes d))]
              :when (and nm (:text nm))]
          (:text nm))))

(defn- asserts-inside? [nodes helpers form]
  (some #(and (= :list (:tag %))
              (or (contains? assertions (:head %))
                  (contains? helpers (:head %))))
        (tree/children-of nodes form)))

(defn findings [nodes]
  (let [helpers (assertion-helpers nodes)]
   (concat
   (for [n (tree/lists-headed-by nodes #{"deftest" "defspec"})
         :when (not (asserts-inside? nodes helpers n))]
     {:rule "empty-test"
      :line (:line n) :col (:col n) :end-line (:end-line n) :end-col (:end-col n)
      :message "this test asserts nothing, so it passes whatever the code does"})

   (for [n (tree/lists-headed-by nodes #{"testing"})
         :when (not (asserts-inside? nodes helpers n))]
     {:rule "testing-without-assertion"
      :line (:line n) :col (:col n) :end-line (:end-line n) :end-col (:end-col n)
      :message "this `testing` block contains no assertion"})

   (for [n (tree/lists-headed-by nodes #{"is"})
         :let [a (tree/first-argument nodes n)]
         :when (and a (= :list (:tag a))
                    (contains? #{"=" "not=" "==" "<" ">" "<=" ">="} (:head a))
                    (= 1 (count (tree/arguments nodes a))))]
     {:rule "test-with-no-effect"
      :line (:line n) :col (:col n) :end-line (:end-line n) :end-col (:end-col n)
      :message (str "(" (:head a) " x) with one operand is always true, so this assertion"
                    " cannot fail")}))))
