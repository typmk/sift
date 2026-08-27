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

**The host boundary, as far as the source states it** — `host.cljc`:
`catch-all-swallow`, `mutable-escape`, `js-prop-on-own-object` (counterpart
`(aget o "k")`), `reflection-unwarned`. What the source cannot state, the
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
element. It emits `(some (fn [x] (when (p x) x)) xs)` now.

**Every finding carries** `:rule`, `:category` (Credo's `:refactor`
`:readability` `:design` `:warning` `:consistency`), `:instruction`, and an
`:applicability` on clippy's four rungs — `:machine-applicable`,
`:maybe-incorrect`, `:has-placeholders`, `:unspecified` — so an editor knows
what it may apply unasked. `shape/rules` is the registry; `zip.cljc` is the
one zipper vocabulary every rule reads the tree with.

## The CLI

    bin/sift shape <path>…                    # findings, exit 1 if any
    bin/sift complexity [--threshold N] <path>…
    bin/sift shape --edn src                  # as data
    bin/sift shape --analysis kondo.json src  # resolve through clj-kondo
    bin/sift shape --write-baseline b.edn src # freeze today's findings …
    bin/sift shape --baseline b.edn src       # … and report only new ones

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

95 tests, 319 assertions. `sonar-clojure` runs the same files a second time
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
