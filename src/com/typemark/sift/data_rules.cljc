(ns com.typemark.sift.data-rules
  "The one place rules.edn is read. A macro, so the file is inlined at
  compile time on every host — the JVM, babashka, and ClojureScript, where
  there is no classpath to read at runtime — and a rule file that does not
  parse fails the BUILD rather than the first scan."
  #?(:clj (:require [clojure.edn :as edn]
                    [clojure.java.io :as io])))

#?(:clj
   (defmacro load-edn
     "Any EDN resource under com/typemark/sift, inlined at compile time — hosts.edn
     and concepts.edn ride the same mechanism as rules.edn."
     [resource-name]
     (let [r (or (io/resource (str "com/typemark/sift/" resource-name))
                 (throw (ex-info (str resource-name " is not on the classpath") {})))]
       `(quote ~(edn/read-string (slurp r))))))

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
         (assert (contains? #{:compiler :parity :corpus :read :unjudged} (:evidence rule)) (str id " has no :evidence rung"))
         ;; :head-ns names a class whose MEMBERS are wild, so it carries no
         ;; pattern — clojure.lang.RT/* is the whole of Clojure's internals
         ;; and enumerating its members is not a rule.
         (assert (or (some? match) (vector? (:either rule)) (string? (:head-ns rule)))
                 (str id " has no :match, :either or :head-ns"))
         (when (= kind :rewrite)
           (assert (contains? rule :emit) (str id " is :rewrite and has no :emit"))))
       `(quote ~rules))))
