(ns net.typemark.sift.forms
  (:require [clojure.set :as set]))

(def branch
  #{"if" "if-not" "if-let" "if-some" "when" "when-not" "when-let" "when-some"
    "when-first" "cond" "cond->" "cond->>" "condp" "case" "and" "or" "while"
    "catch" "some->" "some->>"})

(def function
  #{"defn" "defn-" "defmacro" "defmethod" "defmulti" "fn" "fn*" "definline"})

(def type-def
  #{"deftype" "defrecord" "defprotocol" "definterface" "reify" "proxy"
    "defstruct" "gen-class"})

(def ^:private structural
  #{"def" "do" "let" "let*" "letfn" "loop" "recur" "new" "quote" "var" "set!"
    "throw" "try" "finally" "monitor-enter" "monitor-exit" "." "defonce"
    "declare" "ns" "in-ns" "require" "import" "use" "refer" "binding"
    "doseq" "dotimes" "for" "as->" "->" "->>" "doto" "with-open" "with-meta"})

(def special
  (set/union branch function type-def structural))
