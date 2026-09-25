# sift

A static analyser for Clojure and ClojureScript. It computes per-function
complexity, detects imperative control flow expressible as a fold or a
dispatch, and applies prose rules to docstrings. Analysis is over the
rewrite-clj parse tree; source is not evaluated. Symbol resolution, where
required, is taken from clj-kondo's analysis output. The source is `.cljc`
and yields identical findings on the JVM, babashka and ClojureScript (node).

**Status: alpha.** sift is a library, consumed by a code-graph tool and a
SonarQube plugin. The API, rule identifiers and finding schema are unstable.
`bin/sift` exists for testing and is not supported:

    bin/sift lint src                  # babashka
    clojure -M:sift lint src           # JVM
    bbin install io.github.typmk/sift  # then: sift lint src

`sift rules` lists the rules; `sift lint` without arguments prints the
options.

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

## Scope

**Complexity.** Cognitive complexity [Campbell 2017] and cyclomatic
complexity [McCabe 1976], per function and per nested function, against a
configurable maximum. Cognitive complexity penalises nesting and breaks in
linear flow, which a branch count does not. On 5,531 functions of a code-graph tool it
agrees with cccc-core on 5,519; the 12 differences are lambdas inside
`(comment …)`, which sift treats as data.

**Imperative forms.** Each finding carries a rewrite and an applicability
level [Clippy]:

| form | rewrite |
|---|---|
| atom as accumulator, `swap!` in a loop | `reduce` / `into` / `group-by` |
| `loop`/`recur` conjoining onto a vector | `(into [] (map f) xs)` |
| `loop`/`recur` threading an accumulator | `reduce` |
| `cond` comparing one value to literals | `case` |
| `(first (filter p xs))` | `(some (fn [x] (when (p x) x)) xs)` |
| one name conditionally rebound three or more times in a `let` | `cond->` |
| a `let` whose every binding feeds the next and is returned | `->` / `->>` |
| catch-all `catch` returning a constant | failure as data |
| function returning a host mutable (`ArrayList`, `#js`) | a value |

The `some` rewrite retains the element; `(some p xs)` returns the
predicate's value. The two differ when the element is `nil` or `false`,
so the fix is marked `maybe-incorrect`. Expression-level idiom is out of
scope and left to Splint.

**Docstrings.** Rules flag a docstring that restates the var name, hedges,
names none of its parameters, or is a placeholder. On a code-graph tool's source:
2, 27, 142 and 2 findings respectively. Docstrings are read from the source,
not from clj-kondo. `:doc/docstring` and `:doc/comment`, off by default,
report every docstring, comment, `#_` form and `(comment …)` block with its
exact span, for a gate or a tool that removes them. `sift docs-mirror`
exports docstrings and comments at their source lines for Vale;
`sift vale-sarif` converts Vale's output to SARIF.

**Other rules.** Reflection and boxed arithmetic predicted from source,
scored against the compiler by `bin/validate` (requires `--oracle`).
Security sinks at host APIs: XXE, JNDI, deserialisation, trust-all TLS,
ReDoS, CSRF, cookie flags, secrets in logs, and inter-procedural taint
(requires clj-kondo). Blocking operations inside `go`, `:else` in `case`,
`require` outside the `ns` form, `clojure.lang.RT` calls, nested reference
types. Tests without assertions. Tenancy rules, off by default.

## clj-kondo

Rules whose logic needs only the form and its position also ship as a
clj-kondo export, `clj-kondo.exports/net.typemark/sift`: cognitive
complexity and `cond-as-case` so far, from the same code sift runs. A
project that depends on sift picks them up with

    clj-kondo --lint "$(clojure -Spath)" --copy-configs --skip-lint

and clojure-lsp then shows them in the editor. Complexity is scored per
defining form; a project's own `def…` macros need a hook line of their own
(`{:hooks {:analyze-call {my.ns/deftool net.typemark.sift.kondo/cognitive-complexity}}}`)
or their bodies are scored as an anonymous `fn`. The suite fails if the
exported code drifts from `src`, or if the hooks and sift disagree on the
test corpus.

## Evidence

Each rule declares the strongest check it has passed: `compiler` (agrees
with the compiler), `parity` (agrees with another tool), `corpus` (flag and
clear fixtures), `read` (every hit on real code inspected), or `unjudged`.

## Configuration

A rule extends one of four checks (`existence`, `substitution`, `metric`,
`script`) with data: a pattern, a template, a maximum. A ruleset is the
namespace of a rule id. `.sift.edn` selects rulesets, sets levels, and
defines rules; naming a rule in `:rules` enables it regardless of ruleset.

```clojure
{:rulesets #{:correctness :security :complexity}
 :rules {:complexity/loop-as-map :off
         :complexity/cognitive-complexity {:max 20}
         :house/no-println {:extends :existence :match (println ?&_)
                            :message "use the logger"}}}
```

Library entry point: `(sift/lint (sift/linter config) [{:path … :text …}])`.

## References

- G. A. Campbell. *Cognitive Complexity: A New Way of Measuring
  Understandability.* SonarSource, 2017.
- T. J. McCabe. "A Complexity Measure." *IEEE Trans. Softw. Eng.* SE-2(4),
  1976.
- T. Lindahl, K. Sagonas. "Practical Type Inference Based on Success
  Typings." PPDP 2006. Report only what is certain to fail.
- M. Fowler, K. Beck. *Refactoring.* Addison-Wesley, 1999. Smells paired
  with refactorings.
- [Vale](https://vale.sh): checks extended by rules, selected by config.
- [clj-kondo](https://github.com/clj-kondo/clj-kondo): levels, and the
  analysis sift reads for resolution and the call graph.
- [Splint](https://github.com/NoahTheDuke/splint): `genre/rule` naming.
- [Semgrep](https://semgrep.dev/docs/writing-rules/rule-syntax): pattern
  combinators `either`, `not`, `inside`.
- [HLint](https://github.com/ndmitchell/hlint): rules as match and
  replacement.
- [Clippy](https://doc.rust-lang.org/clippy/): applicability levels and
  rule categories.
- [detekt](https://detekt.dev/docs/introduction/baseline/): baselines.
- [rewrite-clj](https://github.com/clj-commons/rewrite-clj): the parser.
- [SARIF 2.1.0](https://docs.oasis-open.org/sarif/sarif/v2.1.0/sarif-v2.1.0.html):
  output format.
