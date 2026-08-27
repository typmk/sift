# sift

Clojure analysis: a rewrite-clj node stream, rules over it, rules over
clj-kondo's analysis output, and the measures derived from either.

No SonarQube in it, and no JVM in it. `.cljc` throughout, so it runs on the JVM
inside sonar-clojure's plugin and as ClojureScript inside defnet — verified by
running it on both, not by compiling it.

    (require '[com.typemark.sift :as sift])
    (let [{:keys [nodes]} (sift/parse-source source)]
      {:findings (sift/findings nodes)
       :measures (sift/measures nodes)})

**One call, one shape.** A consumer that wants everything sift can say
about a file calls `analyze` and filters on `:family`:

    (sift/analyze {:text source :path "src/a.clj"
                   :resolution (sift/resolution kondo-json)      ; optional
                   :prose      (sift/prose-findings kondo-json)}) ; optional
    ;; => {:ok? true
    ;;     :findings [{:rule :place-as-fold :family :shape :category :refactor
    ;;                 :applicability :machine-applicable :instruction "…"
    ;;                 :line 5 :message "…" :counterpart (reduce …)} …]
    ;;     :units    [{:name "f" :cognitive 3 :cyclomatic 3 :children […]} …]
    ;;     :seeds    {…}}

Every finding, whichever family produced it — `:node` (the stream rules),
`:shape`, `:complexity`, `:prose` — carries a keyword `:rule`, a
`:category`, an `:applicability` and an `:instruction`. defnet's
`ingest op=scan` is one fold over this; `bin/sift lint` prints it. The
per-family functions below still exist for sonar-clojure.

Two more kinds of rule run over source TEXT rather than the node stream,
because they need the tree:

    (sift/unit-complexity source path)     ; per-unit cognitive, cyclomatic,
                                           ; max nesting, params, children
    (sift/complexity-findings source path) ; units over 15 (Sonar's default)
    (sift/shape-findings source path)      ; place-as-fold, loop-as-map,
                                           ; loop-as-reduce, cond-as-case
    (sift/shape-findings source path (sift/resolution kondo-analysis-json))

`complexity` follows Campbell's paper (SonarSource 2017) and was measured
identical to cccc-core on 5,519 of 5,531 units of defnet; the 12 are lambdas
inside `(comment …)`, which this treats as data. `shape` is the `places` rule
set from agentia — an atom used as a fold, a loop that is a map — plus a
loop that threads an accumulator (a `reduce`) and a `cond` over literals (a
`case`), with its corpus as the spec under `test/…/corpus/{clear,flag,resolved}`.
A rule enters only with a flag file that fires and a clear file that stays
silent; and every rule is run over real code before it lands — the four fire
0 / 0 / 0 / 1 times on defnet's own source and 3 / 0 / 1 / 7 on clojure.core,
each hit read. A `cond` with no `:else` gets a `case` with an explicit `nil`
default, because the shorter rewrite throws where the original returned nil.

**sift never resolves a symbol; clj-kondo does, and sift reads it.** With an
analysis (`clj-kondo --config '{:analysis {:locals true} :output {:format
:json}}'`) the shape rules know that `(c/atom …)` is `clojure.core/atom` and
that a parameter named `swap!` is not the mutator. Measured on the fixture
under `corpus/resolved`: without it the aliased atom is missed and the
parameter is flagged; with it, the reverse. Measured on defnet: no change in
17 findings — the input is there for codebases that need it, not because this
one did.

**The host boundary, as far as the source states it** — `catch-all-swallow`
and `mutable-escape` in `host.cljc` (shape questions), `js-prop-on-own-object`
(counterpart `(aget o "k")`) and `reflection-unwarned` in `typeflow.cljc`
(tag questions, on the same env as the predictions — and the move found
that the old rule read only the member name and missed every `Class/static`). What the source cannot state, the
host compiler writes with positions, and defnet's `ingest op=hostwarn` reads
that log. On first contact `js-prop-on-own-object` found a `(set! (.-k props)
…)` on a `#js` literal in defnet's viewer — the `:advanced` rename trap it
had already shipped once.

**Rules as data — `rules.edn`.** Vale's shape: a few rule KINDS, each
configured by data, rather than one language expected to say everything.
`:rewrite` is a `:match` pattern and an `:emit` template; `:forbid` is a
pattern and a message. Patterns are `pattern.cljc`'s — the vocabulary
defnet's `edit op=pattern` already speaks: `?x` binds and unifies, `?_`
ignores, `?&rest` takes the tail, `P ...` repeats, `(?? P Q) ...` repeats a
group — so `(cond (?? (= ?x ?k) ?e) ... :else ?d)` walks the pairs and
`(case ?x (?? ?k ?e) ... ?d)` writes them back. `:when` names guards. The
file is inlined at compile time by a macro, so it is one file on the JVM,
under babashka and in ClojureScript, and a rule that does not parse fails
the build. First match wins per form; order the file specific to general.
A rule that needs the zipper, positions across forms or a count stays a
coded rule and is registered beside these — the format does not decide what
is a rule, the corpus does, and every data rule has its flag and clear
lines in `corpus/{flag,clear}/data_rules*.clj`.

Reading the hits on real code before landing is the gate that catches what
the corpus cannot: `(first (filter p xs))` → `(some p xs)` matched ten
times on defnet and every rewrite would have returned `true` instead of the
element. It emits `(some (fn [x] (when (p x) x)) xs)` now. And the file is
short on purpose: six data rules were splint's under other names
(`lint/if-else-nil`, `lint/if-not-both`, `style/when-not-call`,
`style/eq-zero`, `lint/not-empty?`, `style/apply-str-interpose`) and were
removed once measured; splint is in the estate's `lint:style`, and sift's
ground is shape, complexity, the host boundary and the graph, not idiom.

**Where, not only what — Semgrep's combinators in `rules.edn`.** `:either
[P …]` in place of `:match`, `:not [P …]` to exclude, `:inside P` for an
ancestor that must match with the SAME bindings — so `(deref ?a)` `:inside`
`(swap! ?a ?&_)` is the atom read while its own swap is computed, a lost
update, and a deref of a different atom is not. Measured: one, in lume's
`secrets.clj`, the argument form `(swap! a assoc k (merge (get @a k) …))`.
A reader form is a form: `@a` is a `:deref` node and reads as
`(clojure.core/deref a)`, and until the engine collected by sexpr rather
than by `z/list?` no rule could see it.

**Docstrings, judged as Vale judges prose — `prose.cljc`.** Over clj-kondo's
analysis (`{:analysis {:arglists true}}`), which already carries every
var's `doc`, `arglist-strs` and span and every namespace's `doc`, so there
is no parser: `:doc/restates-name` ("Parses the config." on `parse-config`),
`:doc/hedge` ("this function…", "is used to", "simply"), `:doc/params-unnamed`
(a short doc for two or more parameters that names none of them, nor any
piece of one), `:doc/placeholder`, `:doc/ns-missing`. The corpus is
`corpus/prose/docs.clj` with the analysis kondo emitted beside it. On
defnet: `restates-name` 7, all real; `hedge` 30; `params-unnamed` 133 —
measured at 272 before the length guard, every extra one a long docstring
that explains the design and never says `from-name`, which is not the
smell. `sift/prose-findings` returns these merged with the banned-term
dictionary.

**Vale itself, with no new reader and no new surface.** `bin/sift docs-mirror
analysis.json out/` writes each source file as `<path>.md` holding only its
docstrings at their original lines; Vale lints that (`vale/.vale.ini`, styles
under `vale/styles/Sift` — Hedge, Placeholder, Weasel — or any pack you add);
`bin/sift vale-sarif vale.json` turns Vale's JSON into SARIF with the `.md`
stripped, so defnet's existing `ingest op=sarif` attributes every alert to
the definition by file and line. Measured on defnet: 42 alerts, 42
attributed, 84 labels (`:vale/Sift.Hedge` + level), zero defnet code
changed. Vale's `[formats]` mapping refused `.cljs`, which is why the mirror
carries the suffix.

**Type flow — where a tag dies, predicted, and judged by the host compiler
(`typeflow.cljc`).** Clojure on a host is an overlay: every value is Object,
every fn is `IFn.invoke(Object…)`, and the only static types are hints,
literals, casts and host signatures. The compiler carries a tag locally and
takes the slow path where it runs out — reflection and boxed math on the JVM
— and says so. `typeflow` predicts those sites from the source alone: params
(hinted or Object), literals, `let`/`loop`/`with-open`/`if-let` bindings,
casts, core fns the host knows, `:arith` (primitive iff every operand is),
`doto`/`->` threaded receivers, `catch` classes, statics and constructors as
known classes, and a short table of overloaded members. Dialyzer's stance:
never *this is well-typed*, only *the compiler will not know this tag here*.
`hosts.edn` makes a dialect a row (JVM, JS, …); `concepts.edn` (vendored
from clojure-runtime-book) is the lattice above the host.

**Judged before believed — `bin/falsify`, against assay's notes:**

| corpus | boxed-math | reflection |
|---|---|---|
| sift itself (JVM) | 44 / 45 — P 0.98 R 0.98 | oracle empty (fully hinted) |
| agentia (JVM) | — | 1 / 1 |
| lume (JVM, 334 notes), text alone | P 0.87 R 0.98 | P 0.68 R 0.96 |
| lume, with resolution + var tags | P 0.87 R 0.98 | **P 0.81 R 0.96** |
| defnet (JS, Closure) | n/a | **648 predicted, 0 warned — the model was wrong, and JS predicts nothing** |

The step from 0.68 to 0.81 is **return tags on vars**, which a text pass
cannot see and the compiler reads: the corpus's own `(defn ^Tag f …)` /
`(defn f ^Tag […])` via `sift/var-tags` over its trees, and libraries' via
`bin/var-tags` on the JVM — `(:tag (meta v))` *or the first arglist's tag*,
because `clojure.data.json/write-str` keeps its `^String` on the arglist and
none of its vars carry one. Both join at the call form's position through
kondo's resolution. clj-kondo itself strips hints (`arglist-strs` gives
`[s]` for `[^String s]`), which is why the trees are read. Occurrence typing
(Tobin-Hochstadt & Felleisen, `:narrows` in `hosts.edn`) is in and correct
and moved lume by one finding — it rarely guards interop with a predicate.

What the misses are: a bare `inc` inside `cond->` (threading expansion),
and a receiver bound by destructuring. Every rule in `hosts.edn` was learned
from a false positive or a miss on real code and says which. defnet's
`ingest op=scan` runs two passes so cross-file return hints count and lands
these as `:sift/boxed-math` / `:sift/reflection` on definitions;
`op=hostwarn` lands the compiler's own answer beside them.

**Every finding carries** `:rule`, `:category` (Credo's `:refactor`
`:readability` `:design` `:warning` `:consistency`), `:instruction`, and an
`:applicability` on clippy's four rungs — `:machine-applicable`,
`:maybe-incorrect`, `:has-placeholders`, `:unspecified` — so an editor knows
what it may apply unasked. `shape/rules` is the registry; `zip.cljc` is the
one zipper vocabulary every rule reads the tree with.

## The CLI

    bin/sift lint [--family F] <path>…        # everything, one shape; exit 1 if any
    bin/sift shape <path>…                    # the shape family only
    bin/sift complexity [--threshold N] <path>…
    bin/sift shape --edn src                  # as data
    bin/sift shape --analysis kondo.json src  # resolve through clj-kondo
    bin/sift shape --write-baseline b.edn src # freeze today's findings …
    bin/sift shape --baseline b.edn src       # … and report only new ones
    bin/sift docs-mirror analysis.json out/   # docstrings as <path>.md, for Vale
    bin/sift vale-sarif vale.json > vale.sarif # then: defnet ingest op=sarif
    bin/sift vale-style vale/styles/Sift      # regenerate the styles from prose.cljc

A babashka script over the same namespaces, for a lint task or a prompt. defnet's
`ingest op=scan` is the same rules attached to graph definitions.

## Why its own repo

Two consumers on two runtimes. Putting it inside either reproduces an approach
already killed with reasons in defnet's decision log
(`where-should-sonar-clojure-live`): *makes a published product a component of
one of its consumers*.

## Two rules for anyone editing this

**No var on `sift` may share a name with a child namespace segment.** On the JVM
`com.typemark.sift/parse` and the namespace `…sift.parse` coexist. In
ClojureScript they compile to the same JavaScript path and one silently
overwrites the other. That is why the entry point is `parse-source`.

**Java class names here are DATA, not dependencies.** `interop` and `security`
name `ProcessBuilder`, `MessageDigest`, `clojure.java.shell` and friends because
they detect them in the code under analysis. Nothing here imports them, and
nothing should.

## Running the tests

    clojure -M:test

103 tests, 351 assertions. `sonar-clojure` runs the same files a second time
through its own `:test` alias (`-d ../sift/test`): this harness proves the
library stands alone, that one proves it still fits the consumer.

## History

Extracted from `hbtweb/sonar-clojure`, which carries the commits from before the
split — this repo starts at the extraction.

Named `scan` while it was a directory nobody else could reach. `scan` is a verb
and a generic one, and it made the collision rule above hard to hold: every
child namespace here — `parse`, `security`, `metrics` — is also a name you would
want as a var on the facade, and one of them (`parse`) had already been renamed
to `parse-source` because of it. `sift` is a noun with no such namesakes. The
rename happened at publication, when the name became a URL, a coordinate and a
require in two repositories rather than a path on one laptop.

## Licence

Eclipse Public License 2.0 — the same licence it carried inside sonar-clojure,
where these files lived for 38 commits. The extraction moved the code and left
the LICENSE file behind; this restores it rather than choosing anew. It matters
because defnet's free tier is EPL-2.0 too and compiles this in, so an artifact
containing unlicensed code would have shipped with no grant covering it.
