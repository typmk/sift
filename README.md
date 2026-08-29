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

**Judged before believed — `bin/validate`, against the host compiler.**
`bin/oracle`, run where a project loads, writes what the JVM knows — and,
since 2026-08-29, `definitions.edn`: every var the image holds in a project
namespace and every var in `user` with no `:file`, which is a definition
evaluated at a REPL. defnet's `ingest op=scan file=<dir>` lands those as
`:add-fn` with `:file nil`, the one honest producer for that event, and
classes them `:repl-only` in `by=unused`. Protocol-method vars have no
`:file` either and are told apart by `:protocol` on the meta; clojure.core's
RT-interned vars (`*ns*`, `in-ns`) are not dumped at all:
assay's reflection and boxed-math notes, the list of files that actually
compiled, every var's return tag, and a class table — supertypes, fields,
constructors and methods with parameter types and whether a public class
declares them — for every class the corpus imports, hints, calls or
constructs and what those return. `typeflow` then runs the compiler's own
rules over it: `Compiler.paramArgTypeMatch` and `getMatchingParams`, one
method at an arity taken without looking, `getAsMethodOfPublicBase`, a
`NewExpr` keeping its class whether or not the constructor resolved, a
constant `["a" "b"]` being a `PersistentVector` (a `List`) where `["a" x]`
is an `IPersistentVector` (not one), `true` being a `Boolean`, `doseq`
binding an element the compiler never types, `..` steps with no dot, a
`^Hint` on a `->` step, a nested `cond->` starting from the threaded value,
`with-open`'s implicit `.close`, and a simple name with two classes behind
it resolved by the file's `:import`. Every one of those was a miss or a
false positive on real code first, and every one is a test.

| corpus | first score, blind | now | 
|---|---|---|
| lume (91 files, 335 notes) | in-sample — learned from | boxed **106 / 106**, reflection **26 / 26** |
| sift (32 files, 102 notes) | held-out | boxed **56 / 56**, reflection: oracle empty, 0 predicted |
| agentia `lib/` (3 files) | held-out | boxed **16 / 16**, reflection **3 / 3** |
| clojure-mcp (74 files, 517 notes) | boxed 79/79 · reflection **P 0.92 R 0.75** | boxed **79 / 79**, reflection **211 / 211** |
| darling-toolkit (29 files, 71 notes) | boxed 38/39 · reflection **P 0.85 R 0.85** | boxed **39 / 39**, reflection **20 / 20** |
| kora.core (22 files, 3,080 notes) | boxed **P 0.82 R 0.99** · reflection **P 0.71 R 0.88** | boxed **3,055 / 3,055**, reflection **25 / 25** |
| defnet viewer at `92270f9^` (JS, 42 Closure warnings) | P 0.16 R 1.00 | **P 0.84 R 1.00** (42 / 50) — Closure's externs, `bin/oracle-js` |

**The forecasts are the "first score" column** — each taken before a line
of that corpus was read. Everything in "now" is a fit. What the three
blind corpora taught was never a corpus's oddity but a compiler fact the
model lacked: one method at an arity is taken without looking at the
argument; a method declared only on a package-private class reflects; an
untyped argument is `Object` and fits only an `Object` parameter; a
constant `["a" "b"]` is a `List` and `["a" x]` is not; a `^List` hint on
a vector literal is a `MetaExpr` and types nothing; `true` is a `Boolean`;
`max`/`min` are nary; `(/ long long)` is a `Number`; `Numbers.add(double,
Object)` returns a `double`; a `double` fits `abs(float)` too and the
compiler takes the exact one; a simple name with two classes behind it is
the file's `:import`; every top-level form compiles, `(register-converter
:k (fn …))` included; each arity of a `defn` carries its own hints; and
**the compiler never narrows on a predicate** — occurrence typing, which
had moved lume by one finding, is retracted. Each is one test. And twice
the instrument was wrong before the model was: five of lume's misses were
files the compiler never compiled, and kora's first oracle had lost 2,300
notes to assay's 256 KB sink and read as P 0.25. `corpora/*.edn` carries
each corpus's role and first score, and a role only ever moves one way.

**The JS host, judged for the first time.** The 648-vs-0 that switched
it off was an EMPTY oracle — defnet's source was already clean — judging
a model. The viewer at `92270f9^`, the last commit before its 42
`:infer-warning`s were fixed, is the oracle now (`corpora/defnet-viewer-old.edn`,
a worktree). `cljs.analyzer/analyze-dot` warns when the target's inferred
tag is `nil` or `any`; shadow-cljs's `:infer-externs :auto` then keeps only
the properties Closure's default externs do not declare — `beginPath` on an
untyped `ctx` is silent, `sameNs` is not. `bin/oracle-js` dumps that set
(6,634 names) from the Closure jar; string-required aliases (`["d3" :as
d3]`, `:refer [Graph]`) are `js`. P 0.16 → 0.68 → **0.84, R 1.00**. The 8
false positives left (`radius`, `strength`, `x0`, `y0`, `leaves`,
`metrics`, `isSimulationRunning`) are names shadow knows and this does
not: adding every property name from the npm sources the build requires
reclaimed those 8 and LOST 14 true ones (`zoomIn`, `setProps`,
`sourcePosition` — all in npm sources too), so that theory is wrong and
`bin/oracle-js --sources` stays only as its record. What separates the two
sets is in shadow's own source, which is AOT-only in the local jar and
was not read.

**The annotated walk, and rules that ask what a receiver is.** The two
traversal kits stay — 130 lines of `tree.cljc` against a 960-line port, and
sonar's sensor needs the node stream for line data and CPD — but the cost
that mattered is gone: `typeflow/tags` is the walk's tag environment made
addressable by position, and the data engine reads it. A rule in
`rules.edn` may now guard a bound form with `{:tag "javax.naming.Context"}`
(the form's tag is that class or a subtype in the oracle's table) or
`:dynamic` (computed, not a literal), and its head names a class in full —
`(InitialContext/doLookup n)` is resolved through the file's `:import`
before matching, `(ProcessBuilder. c)` through `java.lang`. Four of
`interop.cljc`'s detections became four rows: `jndi-injection` covers the
instance form `(.lookup ctx n)` for the first time, and only when `ctx`'s
tag is known — an untyped receiver is no claim, not a guess. What stays in
code is what needs a walk of its own (the XML hardening scan, the
`reify`d TrustManager).

**The oracle is the architecture.** `hosts.edn` no longer carries a hand
copy of the JDK: `:core` is only what the compiler knows without reading a
var — the `:inline` forms, the casts, the array constructors — and
`:host-returns` and `:overloaded` are deleted. Each had been wrong at least
once before the dump corrected it (`vec`, `keyword`, `Math/abs`, `URL.`).
The tree-read `var-tags` is retired too: `bin/oracle` dumps every interned
var's return hint, `defn-` included (`ns-publics` had dropped those, and
nine of lume's builders came back as false reflections until it was
`ns-interns`), and `^:const` literals as their class. A typeflow finding
produced without an oracle says `:evidence :unjudged` on the finding
itself.

**Evidence lives on the rule.** `evidence.edn` is gone; every registry
entry — shape, data, prose, typeflow, complexity, and a registry for the
node rules — carries `:evidence` and `:note`, `load-rules` refuses a
`rules.edn` row without a rung, and `sift/evidence` derives the ledger.
`bin/validate` prints it: compiler 3 · parity 1 · corpus 35 · read 6 ·
**unjudged 0**.

**The `:inferred` rung has a producer.** `typeflow/inferred` names the
return type of every unhinted `defn` the walk can type; `sift/analyze`
returns it as `:inferred`, and defnet's `op=scan` writes it to the type-fact
table at source `:inferred` — the rung the whole ladder was designed
around and nothing had ever filled. Two arities that disagree are two
facts; `conflicts` reports them.

**One op lands prediction and compilation together.** `op=scan
file=<oracle dir>` now reads `notes.edn` / `closure.log` from the same
directory, lands the compiler's warnings as `:host/<kind>` labels through
`hostwarn`'s reader, and reports `:agreement {:both :predicted-only
:compiled-only}` per definition — the falsifier as a graph fact, on
defnet's own ladder: a note is `observed`, a prediction is `inferred`.

**Run on itself** (`bin/sift lint --oracle ~/.cache/sift/oracles/sift
src`, 2026-08-28): 78 findings — 56 `typeflow/boxed-math` at `:compiler`,
and they are the 56 notes the JVM emits for this source, 0 either way;
10 `cognitive-complexity` at `:parity` (`typeflow`'s `walk` is 223, the
one function that carries the compiler's grammar; `tag-of` 57; pattern's
`match-seq` 48); 9 `first-filter-is-some`; 2 `reflection-unwarned`
(`parse.cljc`, `complexity.cljc`, one interop call each); 1
`cond-as-case`. The run found two defects in the instrument: every
complexity finding carried `:symbol nil` (the name was only in the
message), and `cond-as-case` and a `rules.edn` row fired twice on
`complexity.cljc:379` — one fact, one rule; the row is gone.

**Run on the estate, 2026-08-28.** agentia `lib/`: 26 findings (16 boxed,
3 reflection, all judged; `-main` cognitive 35). sonar-clojure `src/`: 259
— and 225 of them say `:unjudged`, because 14 of its 26 files need the
plugin's classpath and the oracle never compiled them; the 11 judged
boxed operations are the compiler's 11. The first sonar-clojure score
was five false reflections, and every one was the instrument: `bin/oracle`
had thrown on the unloadable namespaces and written no table. defnet's
own cljs: 55 predictions, 0 warnings — the JS row that says how far that
host still is. What the runs changed in sift: a finding in a file the
oracle never compiled says so on itself; an unqualified head is this
namespace's var before core's; taint findings name their ends.

**The files no plain classpath compiles, judged after all.** sonar-clojure's
14 plugin-classpath files got `*warn-on-reflection*` and the plugin's own
suite emitted 201 distinct reflective sites; sift had predicted 200 for
those files and called every one `:unjudged`. Converted to notes as a
second corpus (`sonar-clojure-plugin`, `:kinds ["reflection"]` — that run
cannot warn on boxing): **200 / 201, P 1.00 R 0.995**, and the one miss
is named on its manifest — an enum's `valueOf` on a `^String`-hinted `or`.
A manifest may name which kinds its oracle can judge.

**Four scripts.** `bin/oracle` (JVM: notes, loaded, tags, kondo),
`bin/oracle-js` (Closure's extern names), `bin/validate` (the scorecard;
it judges in-process — `falsify` is folded in; `--residue` prints the
misses), `bin/sift`.

**Text alone is not the claim any more.** Without `bin/oracle`'s table the
walker still runs — hints, literals, casts, `hosts.edn`'s short core and
host-return tables — and predicts less: it cannot know that `.setJdbcUrl`
is alone at its arity or that `maxRetries` is declared on a package-private
base. `evidence.edn` carries `:compiler` for these three rules because the
dump is how they are judged, and `bin/validate` prints SKIPPED, never a
smaller table, for a corpus whose oracle is not on the machine.

**The node families, read the corpus way — no compiler judges these, so a
person did.** Every `:node` finding on lume `src` (91 files), sift and
agentia `lib` was read at its source line, 2026-08-27. Of 68: **13 were
wrong and 4 rules were the cause**, each fixed at the cause and gated —
`permissive-file-permissions` fired on `"401"` / `"403"` as OpenAPI
response keys (4/4; now only a call argument under a mode-setting head,
via `tree/parent`); `hardcoded-credential` on `ceremony-secret-type`
bound to a slug (4/4; a name ending `-type`/`-kind`/`-name`/… names a
kind of secret, not one); `unscoped-tenant-query` on agentia's single-user
ledger (5/5; a CORPUS that never says owner, org or tenant has no tenant
to scope by, and the rule is vacuous there — refused, the boundary
checker's rule; `sift/tenanted?` over the corpus, passed as `:tenanted?`,
because per FILE it silenced a one-query fixture that must fire, and
sonar-clojure's rule tests caught that); `csrf-protection-absent` on
`:delete` as a call argument and as a dispatch-table key (a route method
is a map key inside `["/path" {…}]`; reitit takes a bare handler as the
value, so the value's shape cannot decide it — the same fixtures);
`side-effect-in-swap` on `(reset! decision …)` of a `let`-local atom
inside `swap!` — idempotent under retry, the CAS-with-a-decision idiom.
What remains is a review list, not a defect list: 43 `unscoped-tenant-query`
on lume, 8 `ambiguous-owner-check`, 2 CSRF on token-authenticated routes —
all `:unspecified`, and whether any is a defect needs the data model, which
sift does not hold. That is the honest ceiling for a node rule without a
resolver, and it is why the families that CAN be judged by a compiler are
the ones with numbers above.

**One complexity engine — `metrics.cljc` no longer scores.** Sonar's
file-level `:complexity` and `:cognitive` came from a node-stream count
(`from-nodes`: one per branch node, nesting from depth) that nothing had
ever validated; the unit engine had been measured against cccc (5,519 /
5,531) and SonarJS (Spearman 0.989). `sift/measures` given TEXT now sums
the unit engine over the file, and the difference on sift's own source is
**cyclomatic 819 → 1,145, cognitive 1,617 → 1,359** — the old count was
40% under on one and 16% over on the other. `from-nodes` keeps the size
measures (ncloc, comment lines, functions, classes, statements) and the
node-stream shape for a consumer that has only nodes; sonar-clojure's
sensor passes text now. A top-level form outside any unit is not scored,
as Sonar does not score it.

**How to validate what this README claims.** Three commands, in order of
what they prove: `clojure -M:test` is the gate (110 tests / 470
assertions — corpus flag/clear pairs, every learned compiler fact, and
`evidence_test`, which fails on any registered rule without an entry in
`evidence.edn`); `bin/validate` is the measurement (typeflow against the
compiler on every corpus in `corpora/`, in-sample and held-out labelled,
plus the ledger's rung counts, so the number of rules still `:unjudged` is
printed beside the numbers that are not); and `bin/sift lint` prints each
finding's rung in brackets, so a reader of one line knows whether it rests
on the compiler, a parity run, a corpus, a reading, or nothing yet. A new
corpus is `bin/oracle` in that project plus one `corpora/<name>.edn` with
`:role :blind` — and its first score is the one to keep.

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
