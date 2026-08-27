(ns com.typemark.sift.pattern-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.typemark.sift.pattern :as p]))

(deftest variables-bind-and-unify
  (is (= '{?t x ?a 1} (p/match '(if ?t ?a nil) '(if x 1 nil))))
  (is (nil? (p/match '(if ?t ?a nil) '(if x 1 2))))
  (is (= '{?x a} (p/match '(= ?x ?x) '(= a a))))
  (is (nil? (p/match '(= ?x ?x) '(= a b))) "a second ?x must be the same form")
  (is (= {} (p/match '(f 1) '(f 1))) "a match with no variables is an empty map, not nil"))

(deftest wildcard-rest-and-literals
  (is (= {} (p/match '(catch ?_ ?_ nil) '(catch Exception e nil))))
  (is (= '{?t x ?&body [(a) (b)]} (p/match '(when (not ?t) ?&body) '(when (not x) (a) (b)))))
  (is (nil? (p/match '(Thread/sleep ?&args) '(Thread/yield))) "a symbol without ? is literal")
  (is (= '{?k 1} (p/match '{:a ?k :b 2} '{:b 2 :a 1 :c 3})) "maps match by key"))

(deftest ellipsis-repeats-a-group
  (let [b (p/match '(cond (?? (= ?x ?k) ?e) ... :else ?d) '(cond (= k :a) 1 (= k :b) 2 :else 3))]
    (is (= '[k k] (get b '?x)))
    (is (= '[:a :b] (get b '?k)))
    (is (= '[1 2] (get b '?e)))
    (is (= 3 (get b '?d))))
  (is (= '{?p [] ?d 9} (p/match '(cond ?p ... :else ?d) '(cond :else 9))) "zero repetitions")
  (is (nil? (p/match '(cond (?? (= ?x ?k) ?e) ... :else ?d) '(cond (< k 1) 1 :else 3)))))

(deftest substitute-inverts-match
  (is (= '(when x 1) (p/substitute '(when ?t ?a) '{?t x ?a 1})))
  (is (= '(when-not x (a) (b)) (p/substitute '(when-not ?t ?&body) '{?t x ?&body [(a) (b)]})))
  (is (= '(case k :a 1 :b 2 3)
         (p/substitute '(case ?x (?? ?k ?e) ... ?d) '{?x k ?k [:a :b] ?e [1 2] ?d 3}))))
