# sift

[![ci](https://github.com/typmk/sift/actions/workflows/ci.yml/badge.svg)](https://github.com/typmk/sift/actions/workflows/ci.yml)

Static analysis for Clojure. It finds code that should be written another way,
functions that have grown too complex, host-interop sites where the compiler will
reflect or box, and a set of security and access hotspots. Every finding carries
the evidence it rests on.

- **One library, three runtimes.** Written entirely in `.cljc`, it runs as a JVM
  library, as ClojureScript, and under babashka as a CLI. CI runs every test-corpus
  file through it on the JVM and on node and requires the two outputs to be
  identical.
- **Three dependencies:** Clojure, rewrite-clj, and a JSON reader. sift does not
  resolve symbols itself; it reads [clj-kondo](https://github.com/clj-kondo/clj-kondo)'s
  analysis output when you give it one.
- **Predictions are checked against the compiler.** `typeflow` predicts where the
  JVM compiler will fall back to reflection or boxed math. `sift oracle` records what
  the compiler actually did, and each prediction is judged against it.

## Install

**CLI** (needs [babashka](https://babashka.org)), with [bbin](https://github.com/babashka/bbin):

    bbin install io.github.typmk/sift

or from a clone:

    git clone https://github.com/typmk/sift
    ln -s "$PWD/sift/bin/sift" ~/.local/bin/sift

**Library** (tools.deps git dependency):

    clj -X:deps find-versions :lib io.github.typmk/sift

This prints the `{:git/tag … :git/sha …}` pair for each release. Put it in `deps.edn`:

    {:deps {io.github.typmk/sift {:git/tag "v0.1.0" :git/sha "…"}}}

For ClojureScript, put `src/` on the build's source paths.

## Use

    sift lint src                        # every finding for every file; exit 1 if any
    sift lint --category security src    # refactor | complexity | performance | security | correctness | documentation
    sift lint --edn src                  # findings as data
    sift complexity --threshold 15 src   # units over a cognitive-complexity threshold
    sift lint --write-baseline b.edn src # freeze today's findings …
    sift lint --baseline b.edn src       # … and report only new ones

From Clojure:

    (require '[net.typemark.sift :as sift])

    (sift/analyze {:text (slurp "src/a.clj") :path "src/a.clj"})
    ;; => {:ok? true
    ;;     :findings [{:rule :place-as-fold :category :refactor
    ;;                 :applicability :machine-applicable :evidence :corpus
    ;;                 :line 5 :message "…" :instruction "…" :counterpart (reduce …)} …]
    ;;     :units    [{:name "f" :cognitive 3 :cyclomatic 3 :children […]} …]
    ;;     :inferred [{:name "g" :line 9 :type "java.lang.String"} …]}

sift reports and never rewrites your files. When a rewrite is mechanical, the
finding carries it as `:counterpart`, and its `:applicability` (clippy's four
levels) says whether an editor may apply it unasked.

The CLI prints each finding with its category and evidence:

    src/a.clj:6:16 place-as-fold [refactor, corpus] place used as a fold; the counterpart is reduce
      fix: Do not accumulate in an atom. Use reduce (or into / group-by). …

### Optional inputs

sift runs on source text alone. Three inputs make it see more:

| Input | How to make it | What it adds |
|---|---|---|
| clj-kondo analysis | `clj-kondo --lint src --config '{:analysis {:arglists true :locals true :keywords true} :output {:format :json}}' > analysis.json` · CLI `--analysis analysis.json` | Symbol resolution (`(c/atom …)` is `clojure.core/atom`; a parameter named `swap!` is not the mutator), docstring rules, and banned terms |
| Vocabulary | an EDN file · CLI `--vocabulary vocab.edn` · `:vocabulary` on `analyze` | Your project's own terms: `{:banned {:dimension ":facet"} :shared-ns #{:taxon}}`. `:banned` keywords are reported with what to use instead. `:shared-ns` names attribute namespaces that no tenant scopes, so queries over them aren't flagged |
| Compiler oracle | in *your* project: `sift oracle --out oracle src`, or the alias below as `clojure -M:sift/oracle --out oracle src` · then `sift lint --oracle oracle src` | Each `typeflow` prediction is judged against the compiler's own warnings, with the compiler's class table behind it. Without an oracle, typeflow findings say `[unjudged]` |

The oracle loads your project in a JVM on your own classpath. It is its own
library under `oracle/` with no dependencies, so it adds one namespace and nothing
else. As a `deps.edn` alias:

    :sift/oracle {:extra-deps {io.github.typmk/sift {:git/tag "v0.1.2" :git/sha "…" :deps/root "oracle"}}
                  :main-opts  ["-m" "net.typemark.sift.oracle"]}

## What it finds

53 rules in six categories. `(sift/evidence)` lists each rule with its category and evidence.

| Category | Rules | Examples |
|---|---|---|
| **security** | 17 | shell invocation, XXE, unsafe deserialisation, JNDI lookups, trust-all TLS, ReDoS, hard-coded credentials, CSRF, cookies without flags, secrets logged, queries that name no owner or tenant, taint paths across functions |
| **correctness** | 13 | reading an atom inside its own `swap!` (a lost update), a blocking take inside a `go` block, `case` with `:else` as a constant, a discarded future, `catch Exception` that swallows, a JS property on a `#js` literal that `:advanced` renames, tests that assert nothing |
| **refactor** | 12 | an atom used as a fold → `reduce`; a `loop` that is a `map` or a `reduce`; `cond` over literals → `case`; `(first (filter …))` → `some`; a `let` chain that is `->`; your project's banned terms |
| **performance** | 5 | reflection and boxed math, scored against the compiler; return tags it could not infer; `Thread/sleep` holding a thread |
| **documentation** | 5 | docstrings that restate the name, hedge, leave parameters unnamed or are placeholders; namespaces with no docstring |
| **complexity** | 1 | per-unit cognitive and cyclomatic complexity (Campbell, SonarSource 2017), reported over 15 |

The categories are the concern groups Clippy, PMD and splint use.

`src/net/typemark/sift/rules.edn` holds the rules that are data rather than code.
It has two kinds: `:rewrite` (a pattern and a template) and `:forbid` (a pattern
and a message). Patterns bind with `?x`, skip with `?_`, take the tail with
`?&rest`, and repeat with `P ...`. Semgrep's combinators narrow where a pattern
applies: `:either`, `:not`, and `:inside`, which requires an ancestor to match with
the same bindings. A rule can also guard on the type tag of a bound form. The file
is inlined at compile time, so a rule that does not parse fails the build.

## How findings are judged

Every rule carries the evidence it rests on, and every finding prints it in brackets:

| Rung | Meaning | Rules |
|---|---|---|
| `compiler` | predictions scored against the host compiler's own warnings (`bb validate`) | 3 |
| `parity` | measured equal to an independent implementation | 1 |
| `corpus` | a file under `test/…/corpus/flag` that must fire and one under `clear` that must not | 42 |
| `read` | every hit on real code was read at its line by a person | 7 |
| `unjudged` | none of the above yet, and it says so | 1 |

A rule enters only with a flag/clear pair, and it is run over real code before it
lands. Reading those hits catches what a corpus cannot. For example,
`(first (filter p xs))` → `(some p xs)` matched ten times on one codebase, and
every rewrite would have returned `true` instead of the element. The counterpart
is now `(some (fn [x] (when (p x) x)) xs)`.

Complexity measured the same as cccc on 5,519 of 5,531 units. The 12 that differ
are lambdas inside `(comment …)`, which sift treats as data. Rank correlation with
SonarJS on LightTable is 0.989 (Spearman).

### typeflow against the compiler

Clojure on the JVM types a value only through hints, literals, casts and host
signatures. Where the type (the tag) runs out, the compiler takes the slow path
and warns. `typeflow` predicts those sites from source. It applies the compiler's
own resolution rules (`Compiler.paramArgTypeMatch`, one method at an arity taken
without looking at the arguments, `getAsMethodOfPublicBase`) to the class table
`sift oracle` dumps. What `concepts.edn` and `hosts.edn` state is only what the
compiler knows without reading a var, and the dump supplies the rest.

`bb validate` scores every corpus in `corpora/`. A corpus's role only moves one
way. **Blind** means its first score was recorded before any of its code was read;
that score is the forecast. **In-sample** means rules were learned from it, so its
score is a fit.

Measured with `bb validate` on 2026-09-24:

| Corpus | Role | Boxed math | Reflection |
|---|---|---|---|
| clojure-mcp | in-sample | 79/79 | 211/211 |
| sift | held-out | 72/72 | none warned, none predicted |
| sonar-clojure | held-out | 11/11 | none warned, none predicted |
| cherry | blind | P 1.00 R 0.80 (8 of 10) | none warned, none predicted |
| clojure-lsp `lib/` | blind | P 0.82 R 0.99 (210 of 212, 47 extra) | none warned, 11 predicted |

clojure-mcp was blind at its first score (2026-08-27): boxed 79/79, reflection
P 0.92 R 0.75. Earlier dated records, whose oracles are private or stale:

| Corpus | First score (blind) | Scored 2026-08-28 |
|---|---|---|
| a numeric library (22 files, 3,080 notes) | boxed P 0.82 R 0.99 · reflection P 0.71 R 0.88 | boxed 3,055/3,055 · reflection 25/25 |
| sonar-clojure on its plugin classpath (201 sites) | reflection P 1.00 R 0.995 | stale since; re-dump before believing it |
| a ClojureScript viewer (42 Closure warnings) | P 0.16 R 1.00 | P 0.84 R 1.00, from Closure's externs (`sift oracle --js`) |

An oracle dump goes stale as its source moves. `bb validate` labels a stale one
STALE, and prints SKIPPED for a corpus whose source or oracle is not on the
machine. To reproduce these numbers, check each project out under
`~/.cache/sift/corpora/<name>` and run `sift oracle` in it with
`--out ~/.cache/sift/oracles/<name>`. To add a corpus, do the same and add a
`corpora/<name>.edn` with `:role :blind`. Keep its first score.

## Development

    src/                  the library; src/net/typemark/sift/cli.clj is the CLI (babashka only)
    bin/sift              runs the CLI from a clone
    oracle/               the JVM half of `sift oracle`: a dependency-free library (:deps/root)
    script/               maintainer task code, on bb.edn's :paths
    corpora/              the corpora `bb validate` scores
    test/  test-cljs/     the suite, and the JVM/node parity check

`bb.edn` takes sift itself from `deps.edn`, so the tasks run the same dependency
versions as the JVM suite:

    bb test        # the gate: the unit suite
    bb parity      # every corpus file on the JVM and on node; the outputs must be identical
    bb validate    # the measurement: typeflow against every corpus oracle (needs the oracles)

CI runs `test` and `parity`, plus the CLI under babashka against the flag and clear
corpora, per category.

Two rules for anyone editing this:

- **No var on `net.typemark.sift` may share a name with a child namespace
  segment.** On the JVM `net.typemark.sift/parse` and the namespace
  `net.typemark.sift.parse` coexist. In ClojureScript they compile to the same
  JavaScript path, and one silently overwrites the other. That is why the entry
  point is `parse-source`.
- **Java class names in the rules are data, not dependencies.** `interop` and
  `security` name `ProcessBuilder`, `MessageDigest` and `clojure.java.shell`
  because they detect them in the code under analysis. Nothing here imports them.

## Licence

Eclipse Public License 2.0.
