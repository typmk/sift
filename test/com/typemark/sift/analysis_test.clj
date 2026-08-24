(ns com.typemark.sift.analysis-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.typemark.sift.analysis :as analysis]
            [com.typemark.sift.highlight :as hl]
            [com.typemark.sift.parse :as parse]))

(def report
  (str "{\"analysis\":{"
       "\"var-definitions\":["
       "{\"filename\":\"probe.clj\",\"ns\":\"probe\",\"name\":\"add\","
       "\"row\":2,\"col\":1,\"end-row\":2,\"end-col\":25,"
       "\"name-row\":2,\"name-col\":7,\"name-end-row\":2,\"name-end-col\":10}],"
       "\"var-usages\":["
       "{\"filename\":\"probe.clj\",\"to\":\"probe\",\"name\":\"add\",\"from\":\"probe\","
       "\"row\":3,\"col\":28,\"end-row\":3,\"end-col\":37,"
       "\"name-row\":3,\"name-col\":28,\"name-end-row\":3,\"name-end-col\":31}],"
       "\"locals\":["
       "{\"filename\":\"probe.clj\",\"id\":1,\"name\":\"n\","
       "\"row\":3,\"col\":22,\"end-row\":3,\"end-col\":23}],"
       "\"local-usages\":["
       "{\"filename\":\"probe.clj\",\"id\":1,\"name\":\"n\","
       "\"row\":3,\"col\":32,\"end-row\":3,\"end-col\":33,"
       "\"name-row\":3,\"name-col\":32,\"name-end-row\":3,\"name-end-col\":33},"
       "{\"filename\":\"probe.clj\",\"id\":1,\"name\":\"n\","
       "\"row\":3,\"col\":34,\"end-row\":3,\"end-col\":35,"
       "\"name-row\":3,\"name-col\":34,\"name-end-row\":3,\"name-end-col\":35}]"
       "}}"))

(deftest builds-a-symbol-table-per-file
  (let [syms (get (analysis/symbols report) "probe.clj")]
    (is (= 2 (count syms)) "one local, one var")

    (testing "a var definition anchors on its NAME, not the whole form"
      (let [v (first (filter #(= 7 (:col (:declaration %))) syms))]
        (is (some? v))
        (is (= {:line 2 :col 7 :end-line 2 :end-col 10} (:declaration v)))
        (is (= 1 (count (:references v))))))

    (testing "a let-binding collects both its uses"
      (let [l (first (filter #(= 22 (:col (:declaration %))) syms))]
        (is (some? l))
        (is (= 2 (count (:references l))))
        (is (= #{32 34} (set (map :col (:references l)))))))))

(deftest an-empty-analysis-yields-no-symbols
  (is (= {} (analysis/symbols "{\"analysis\":{}}"))))

(deftest highlighting-classes
  (is (= "COMMENT" (hl/type-of {:type :comment :text "; x"})))
  (is (= "STRING"  (hl/type-of {:type :string :text "\"x\""})))
  (is (= "CONSTANT" (hl/type-of {:type :keyword :text ":x"})))
  (is (= "KEYWORD" (hl/type-of {:type :symbol :text "defn"})))
  (testing "an earmuffed dynamic var reads as a constant"
    (is (= "CONSTANT" (hl/type-of {:type :symbol :text "*out*"}))))
  (testing "an ordinary symbol is left unstyled rather than guessed at"
    (is (nil? (hl/type-of {:type :symbol :text "my-fn"}))))
  (testing "delimiters carry no colour"
    (is (nil? (hl/type-of {:type :delim :text "("})))))

(deftest highlight-spans-cover-only-styled-tokens
  (let [spans (hl/spans (parse/leaves (:nodes (parse/parse "(defn f [] \"s\") ; c"))))]
    (is (= #{"KEYWORD" "STRING" "COMMENT"} (set (map :type spans))))
    (testing "every span carries a usable range"
      (is (every? #(and (>= (:col %) 1) (> (:end-col %) (:col %))) spans)))))
