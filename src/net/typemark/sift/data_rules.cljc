(ns net.typemark.sift.data-rules
  #?(:clj (:require [clojure.edn :as edn]
                    [clojure.java.io :as io])))

#?(:clj
   (defmacro load-edn
     [resource-name]
     (let [r (or (io/resource (str "net/typemark/sift/" resource-name))
                 (throw (ex-info (str resource-name " is not on the classpath") {})))]
       `(quote ~(edn/read-string (slurp r))))))

#?(:clj
   (defmacro load-rules
     []
     (let [r (or (io/resource "net/typemark/sift/rules.edn")
                 (throw (ex-info "net/typemark/sift/rules.edn is not on the classpath" {})))
           rules (edn/read-string (slurp r))]
       (assert (vector? rules) "rules.edn must be a vector of rule maps")
       (doseq [{:keys [id kind match] :as rule} rules]
         (assert (keyword? id) (str "rule without :id: " (pr-str rule)))
         (assert (contains? #{:rewrite :forbid} kind) (str id " has unknown :kind " kind))
         (assert (contains? #{:compiler :parity :corpus :read :unjudged} (:evidence rule)) (str id " has no :evidence rung"))
         (assert (or (some? match) (vector? (:either rule)) (string? (:head-ns rule)))
                 (str id " has no :match, :either or :head-ns"))
         (when (= kind :rewrite)
           (assert (contains? rule :emit) (str id " is :rewrite and has no :emit"))))
       `(quote ~rules))))
