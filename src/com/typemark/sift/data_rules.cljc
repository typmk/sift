(ns com.typemark.sift.data-rules
  "The one place rules.edn is read. A macro, so the file is inlined at
  compile time on every host — the JVM, babashka, and ClojureScript, where
  there is no classpath to read at runtime — and a rule file that does not
  parse fails the BUILD rather than the first scan."
  #?(:clj (:require [clojure.edn :as edn]
                    [clojure.java.io :as io])))

#?(:clj
   (defmacro load-rules
     "rules.edn as a literal vector of rule maps."
     []
     (let [r (or (io/resource "com/typemark/sift/rules.edn")
                 (throw (ex-info "com/typemark/sift/rules.edn is not on the classpath" {})))
           rules (edn/read-string (slurp r))]
       (assert (vector? rules) "rules.edn must be a vector of rule maps")
       (doseq [{:keys [id kind match] :as rule} rules]
         (assert (keyword? id) (str "rule without :id: " (pr-str rule)))
         (assert (contains? #{:rewrite :forbid} kind) (str id " has unknown :kind " kind))
         (assert (some? match) (str id " has no :match"))
         (when (= kind :rewrite)
           (assert (contains? rule :emit) (str id " is :rewrite and has no :emit"))))
       `(quote ~rules))))
