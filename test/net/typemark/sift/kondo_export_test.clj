(ns net.typemark.sift.kondo-export-test
  (:require [clj-kondo.core :as kondo]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [net.typemark.sift :as sift]
            [net.typemark.sift.registry :as registry]))

(def ^:private export "resources/clj-kondo.exports/net.typemark/sift")

(deftest the-exported-rule-code-is-the-source-rule-code
  (testing "a hook may load only files inside the config directory, so the
            portable namespaces are copied there; a copy that drifts is a rule
            that says one thing in sift and another in clj-kondo"
    (doseq [f (file-seq (io/file "src/net/typemark/sift/portable"))
            :when (.isFile f)]
      (let [copy (io/file export "net/typemark/sift/portable" (.getName f))]
        (is (.exists copy) (str "not exported: " (.getName f)))
        (is (= (slurp f) (slurp copy)) (str "drifted: " (.getName f)))))))

(defn- kondo-hits [dir]
  (let [{:keys [findings]} (kondo/run! {:lint [dir]
                                        :config-dir export
                                        :cache false
                                        :config {:output {:format :edn}}})]
    (into #{} (for [f findings :when (#{:sift/cond-as-case :sift/cognitive-complexity} (:type f))]
                [(str (:filename f)) (keyword "sift" (name (:type f))) (:row f)]))))

(defn- sift-hits [dir]
  (let [files (filter #(re-find #"\.clj[cs]?$" (str %)) (file-seq (io/file dir)))
        {:keys [findings]} (sift/lint (sift/linter {:rulesets #{} :rules {:style/cond-as-case :info
                                                                             :complexity/cognitive-complexity :warning}})
                                      (for [f files] {:path (str f) :text (slurp f)}))]
    (into #{} (for [f findings]
                [(:file f) (keyword "sift" (name (:rule f))) (:line f)]))))

(deftest the-clj-kondo-hooks-find-what-sift-finds
  (let [dir "test/net/typemark/sift/corpus"
        k (kondo-hits dir)
        s (sift-hits dir)]
    (is (seq s) "the corpus must trip both rules, or the comparison is vacuous")
    (is (= s k) (str "only sift: " (pr-str (sort (remove k s))) "\nonly clj-kondo: " (pr-str (sort (remove s k)))))))

(deftest static-call-rules-are-config-with-the-registry-s-words
  (let [cfg (edn/read-string (slurp (io/file export "config.edn")))
        msg (fn [id] (:message (first (filter #(= id (:id %)) registry/built-in))))]
    (is (= (msg :performance/thread-sleep)
           (get-in cfg [:linters :discouraged-java-method 'java.lang.Thread 'sleep :message])))
    (is (= (msg :security/predictable-temp-file)
           (get-in cfg [:linters :discouraged-java-method 'java.io.File 'createTempFile :message])))
    (testing "and clj-kondo raises them"
      (let [f (doto (java.io.File/createTempFile "sift-static" ".clj") .deleteOnExit)
            _ (spit f "(defn f [] (Thread/sleep 1) (java.io.File/createTempFile \"a\" \"b\"))")
            {:keys [findings]} (kondo/run! {:lint [(str f)] :cache false :config-dir export
                                            :config {:output {:format :edn}}})]
        (is (= 2 (count (filter #(= :discouraged-java-method (:type %)) findings))))))))
