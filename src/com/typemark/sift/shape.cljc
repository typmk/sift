(ns com.typemark.sift.shape
  "Places used as folds, and loops that are maps — the imperative SHAPE that
  no branch count sees. An atom accumulated over a `doseq` scores 0 cognitive
  and is still the thing an LLM writes when it has stopped thinking in values.

  Two rules, ported whole from agentia's `places` (DEFNET-4), which had them
  as a standalone CLI with a corpus-as-spec harness. The corpus came with
  them: `test/…/corpus/clear` must stay silent and `corpus/flag` must fire,
  on the JVM and under bun. They are zipper rules over source TEXT, the
  third shape of rule this library carries beside the node-stream rules and
  the clj-kondo-analysis rules, and the same shape as `complexity`.

  Each finding carries `:instruction` — the rewrite an agent is told to
  apply — and, when the body is a lone `swap!`/`reset!` on the atom, a
  `:counterpart` form with `:applicability :mechanical`. sift never writes;
  the `--apply` that `places` had belongs to a gated editor, not a linter.

  `^:places/allow` on the binding peels a finding, as it did."
  (:require [com.typemark.sift.fold :as fold]
            [com.typemark.sift.map-loop :as map-loop]
            [rewrite-clj.parser :as parser]
            [rewrite-clj.zip :as z]))

(def rules #{fold/rule map-loop/rule})

(defn findings
  "Source text -> findings, or [] if it does not read as forms. `file` is
  carried onto every finding and is otherwise unused."
  [text file]
  ;; `edn*`, not `of-string`: `of-string` moves to the FIRST form, and a
  ;; walk from there sees only the ns form. Measured: every corpus file
  ;; read as clean.
  (let [zloc (try (z/edn* (parser/parse-string-all text) {:track-position? true})
                  (catch #?(:clj Exception :cljs :default) _ nil))]
    (if zloc
      (into (vec (fold/findings file zloc))
            (map-loop/findings file zloc))
      [])))
