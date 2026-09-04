(ns com.typemark.sift.callgraph-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.typemark.sift.callgraph :as cg]))

(def analysis
  (str "{\"analysis\":{\"var-usages\":["
       "{\"filename\":\"p.clj\",\"from\":\"probe\",\"from-var\":\"handler\","
       "\"to\":\"probe\",\"name\":\"danger\","
       "\"name-row\":4,\"name-col\":22,\"name-end-row\":4,\"name-end-col\":28},"
       "{\"filename\":\"p.clj\",\"from\":\"probe\",\"from-var\":\"handler\","
       "\"to\":\"probe\",\"name\":\"untrusted\","
       "\"name-row\":4,\"name-col\":30,\"name-end-row\":4,\"name-end-col\":39},"
       "{\"filename\":\"p.clj\",\"from\":\"probe\",\"from-var\":\"innocent\","
       "\"to\":\"clojure.core\",\"name\":\"+\","
       "\"name-row\":5,\"name-col\":22,\"name-end-row\":5,\"name-end-col\":23}"
       "]}}"))

(def seeds {:taints #{["probe" "untrusted"]} :reaches #{["probe" "danger"]}})

(deftest builds-a-call-graph-from-from-var
  (let [g (cg/call-graph analysis)]
    (is (= #{["probe" "danger"] ["probe" "untrusted"]}
           (set (map :callee (get g ["probe" "handler"])))))
    (is (= #{["clojure.core" "+"]}
           (set (map :callee (get g ["probe" "innocent"])))))))

(deftest propagation-reaches-a-fixpoint
  (let [{:keys [taints reaches paths]} (cg/propagate (cg/call-graph analysis) seeds)]
    (testing "a caller of a tainting var taints"
      (is (contains? taints ["probe" "handler"])))
    (testing "a caller of a reaching var reaches"
      (is (contains? reaches ["probe" "handler"])))
    (testing "the path is where the two meet"
      (is (= #{["probe" "handler"]} paths)))
    (testing "an unrelated var is on neither side"
      (is (not (contains? taints ["probe" "innocent"])))
      (is (not (contains? reaches ["probe" "innocent"]))))))

(deftest reports-the-crossing-with-a-flow
  (let [fs (cg/findings analysis seeds)]
    (is (= 1 (count fs)))
    (let [f (first fs)]
      (is (= "interprocedural-taint" (:rule f)))
      (is (= "p.clj" (:filename f)))
      (is (re-find #"probe/handler" (:message f)))
      (testing "the flow shows both ends of the path"
        (is (= 2 (count (:flow f))))))))

(deftest a-var-that-is-both-source-and-sink-is-left-to-the-direct-pass
  (testing "reporting it here too would double-count the same defect"
    (let [both {:taints #{["probe" "handler"]} :reaches #{["probe" "handler"]}}]
      (is (empty? (cg/findings analysis both))))))

(deftest no-seeds-means-no-findings
  (is (empty? (cg/findings analysis {:taints #{} :reaches #{}}))))


;; ---------------------------------------------------------------------------
;; A usage inside a protocol implementation or a multimethod arm carries NO
;; from-var, because neither defines a var. Every one of them used to be
;; dropped, which made polymorphic dispatch invisible to taint propagation --
;; in Clojure, exactly where request handling tends to live.

(def poly
  (str "{\"analysis\":{"
       "\"var-definitions\":["
       "{\"filename\":\"h.clj\",\"ns\":\"h\",\"name\":\"tainted\",\"row\":30}],"
       "\"protocol-impls\":["
       "{\"filename\":\"h.clj\",\"impl-ns\":\"h\",\"protocol-ns\":\"api\","
       "\"protocol-name\":\"IRender\",\"method-name\":\"-render\","
       "\"row\":10,\"end-row\":12}],"
       "\"var-usages\":["
       ;; inside the impl body: no from-var, was dropped
       "{\"filename\":\"h.clj\",\"from\":\"h\",\"to\":\"probe\",\"name\":\"danger\","
       "\"row\":11,\"name-row\":11,\"name-col\":5,\"name-end-row\":11,\"name-end-col\":11},"
       ;; the defmethod marker: names the arm, is not itself a call
       "{\"filename\":\"h.clj\",\"from\":\"h\",\"to\":\"h\",\"name\":\"handle\","
       "\"defmethod\":true,\"dispatch-val-str\":\":upload\","
       "\"row\":20,\"name-row\":20,\"name-col\":12,\"name-end-row\":20,\"name-end-col\":18},"
       ;; inside that arm's body: no from-var either
       "{\"filename\":\"h.clj\",\"from\":\"h\",\"to\":\"probe\",\"name\":\"untrusted\","
       "\"row\":21,\"name-row\":21,\"name-col\":5,\"name-end-row\":21,\"name-end-col\":14}"
       "]}}"))

(deftest a-protocol-implementation-owns-the-calls-in-its-body
  (let [g (cg/call-graph poly)]
    (testing "the impl is a node, named for the protocol method it answers"
      (is (contains? g ["h" "IRender/-render"])))
    (testing "and the call inside it is no longer dropped on the floor"
      (is (= #{["probe" "danger"]}
             (set (map :callee (get g ["h" "IRender/-render"]))))))
    (testing "the protocol method a CALLER writes reaches the body that answers it, or the impl is an island nothing can taint"
      (is (contains? (set (map :callee (get g ["api" "-render"])))
                     ["h" "IRender/-render"])))))

(deftest a-multimethod-arm-owns-the-calls-in-its-body
  (let [g (cg/call-graph poly)]
    (testing "the arm is named by its dispatch value, the colon not doubled"
      (is (contains? g ["h" "handle::upload"])
          (str "arms present: " (pr-str (filter #(re-find #"::" (second %)) (keys g))))))
    (testing "and owns the call in its body"
      (is (= #{["probe" "untrusted"]}
             (set (map :callee (get g ["h" "handle::upload"]))))))
    (testing "the multimethod reaches the arm"
      (is (contains? (set (map :callee (get g ["h" "handle"])))
                     ["h" "handle::upload"])))
    (testing "the marker usage is not itself a call, or the arm and its multi form a two-way cycle"
      (is (not (contains? (set (map :callee (get g ["h" "handle::upload"])))
                          ["h" "handle"]))))))

(deftest an-arm-is-bounded-by-the-next-definition
  (testing "h/tainted at row 30 ends the arm, so a call below it is not swept in — the bound is the honest half of an approximation the marker span cannot give exactly"
    (let [g (cg/call-graph poly)
          arm (get g ["h" "handle::upload"])]
      (is (every? #(< (:line %) 30) arm)))))

(deftest taint-flows-through-a-polymorphic-call
  (let [seeds {:taints #{["probe" "untrusted"]} :reaches #{["probe" "danger"]}}
        {:keys [taints]} (cg/propagate (cg/call-graph poly) seeds)]
    (testing "the multimethod taints because the arm it dispatches to does"
      (is (contains? taints ["h" "handle::upload"]))
      (is (contains? taints ["h" "handle"])))))
