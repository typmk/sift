(ns net.typemark.sift.prose-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [net.typemark.sift :as sift]))

(def ^:private fs
  (:findings (sift/lint (sift/linter {:rulesets #{:doc} :rules {:doc/ns-missing :warning}})
                        [{:path "docs.clj" :text (slurp (io/resource "net/typemark/sift/corpus/prose/docs.clj"))}])))

(defn- rules-on [sym] (set (map :rule (filter #(= sym (:symbol %)) fs))))

(deftest the-generated-shapes-fire
  (is (= #{:doc/restates-name :doc/params-unnamed} (rules-on 'parse-config))
      "\"Parses the config.\" on parse-config, naming neither path nor opts")
  (is (= #{:doc/hedge} (rules-on 'total-of)) "this function … is used to … basically … simply")
  (is (= #{:doc/placeholder} (rules-on 'tidy)))
  (is (= #{:doc/ns-missing} (rules-on 'docs)) "\"Utilities.\" is one word"))

(deftest the-hand-written-shape-is-silent
  (is (empty? (rules-on 'well-documented)))
  (is (empty? (rules-on 'undocumented))))

(deftest findings-sit-on-the-docstring
  (is (every? (comp pos-int? :line) fs))
  (is (every? (comp pos-int? :end-column) fs))
  (testing "no clj-kondo analysis is needed"
    (is (not-any? #(= "doc" (namespace (:rule %)))
                  (:skipped (sift/lint (sift/linter {}) [{:path "docs.clj" :text "(ns docs)"}]))))))

(deftest every-docstring-and-comment-is-reported-with-its-span
  (let [src "#!/usr/bin/env bb\n;; one\n;; two\n(defn f\n  \"Doc.\"\n  [x] x) ; tail\n;; kept @keep\n;; run\n#_(g 1)\n(comment (h 2) #_(i))\n"
        fs (:findings (sift/lint (sift/linter {:rulesets #{} :rules {:doc/docstring :warning
                                                                      :doc/comment {:level :warning :allow "@keep"}}})
                                 [{:path "c.clj" :text src}]))
        at (fn [rule] (map #(select-keys % [:line :column :end-line :end-column :kind]) (filter #(= rule (:rule %)) fs)))]
    (is (= [{:line 5 :column 3 :end-line 5 :end-column 9 :kind :var}] (at :doc/docstring)))
    (is (= [{:line 2 :column 1 :end-line 3 :end-column 7 :kind :line}
            {:line 6 :column 10 :end-line 6 :end-column 16 :kind :line}
            {:line 9 :column 1 :end-line 9 :end-column 8 :kind :discard}
            {:line 10 :column 1 :end-line 10 :end-column 22 :kind :rich}]
           (at :doc/comment))
        "the shebang is not a comment, a trailing comment stands alone, and a tagged run is exempt")))

(deftest prose-in-returns-docstrings-and-comments-where-they-are
  (is (= [{:kind :docstring :line 2 :column 3 :end-line 2 :end-column 12 :text "Ns doc."}
          {:kind :comment :line 3 :column 1 :end-line 3 :end-column 13 :text "a comment"}
          {:kind :docstring :line 5 :column 3 :end-line 5 :end-column 10 :text "F doc"}
          {:kind :comment :line 6 :column 10 :end-line 6 :end-column 20 :text "trailing"}]
         (sift/prose-in "(ns m\n  \"Ns doc.\")\n;; a comment\n(defn f\n  \"F doc\"\n  [x] x) ; trailing\n#_(;; not prose\n)"))))

(deftest narration-fires-and-a-stated-contract-does-not
  (let [ids (fn [src] (set (map :rule (:findings (sift/lint (sift/linter {:rulesets #{} :rules {:doc/narrates-body :warning}})
                                                          [{:path "n.clj" :text src}])))))]
    (is (contains? (ids "(defn extract-bb-tasks \"Extracts tasks from bb.edn configuration.\" [config] (get-in config [:bb :tasks]))")
                   :doc/narrates-body))
    (is (empty? (ids "(defn exec \"Execute command, return promise of {:stdout :stderr :code}.\" [command] (run-command command))"))
        "a return shape is a contract")
    (is (empty? (ids "(defn get-module \"Get initialized module or throw.\" [] (or @module (throw (ex-info \"module\" {}))))"))
        "a failure mode is a contract")
    (is (empty? (ids "(defn total \"Sum of prices after discounts, in cents.\" [items] (reduce + (map :price items)))"))
        "words the code does not contain")))
