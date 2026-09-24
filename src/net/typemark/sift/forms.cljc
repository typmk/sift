(ns net.typemark.sift.forms
  "The one place a Clojure form's role is declared.

  Stated twice, these sets drift: `definline`, `defstruct` and `gen-class`
  were counted as functions and classes by the metrics while the highlighter
  left them unstyled, because each namespace kept its own list. `special` is
  derived from the others rather than restated, so a form added below cannot
  be a function to one consumer and an ordinary symbol to another.

  Strings, not symbols: the parser hands us text, and `(symbol text)` on
  arbitrary source is a coercion that can only lose."
  (:require [clojure.set :as set]))

(def branch
  "Forms that introduce a decision. `and`/`or` count because they
  short-circuit; `loop`/`recur` do not, because iteration without a test is
  not a branch."
  #{"if" "if-not" "if-let" "if-some" "when" "when-not" "when-let" "when-some"
    "when-first" "cond" "cond->" "cond->>" "condp" "case" "and" "or" "while"
    "catch" "some->" "some->>"})

(def function
  "Forms that define something callable."
  #{"defn" "defn-" "defmacro" "defmethod" "defmulti" "fn" "fn*" "definline"})

(def type-def
  "Forms that define a type, protocol or named implementation -- what Sonar
  counts under `classes`."
  #{"deftype" "defrecord" "defprotocol" "definterface" "reify" "proxy"
    "defstruct" "gen-class"})

(def ^:private structural
  "Special forms that neither branch nor define: binding, sequencing,
  namespace manipulation, interop."
  #{"def" "do" "let" "let*" "letfn" "loop" "recur" "new" "quote" "var" "set!"
    "throw" "try" "finally" "monitor-enter" "monitor-exit" "." "defonce"
    "declare" "ns" "in-ns" "require" "import" "use" "refer" "binding"
    "doseq" "dotimes" "for" "as->" "->" "->>" "doto" "with-open" "with-meta"})

(def special
  "Everything the highlighter renders as a language keyword. Derived, so it
  cannot fall behind the sets above."
  (set/union branch function type-def structural))
