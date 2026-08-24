(ns au.com.heisenbergtech.scan
  "The analysis surface, named in one place.

  Everything under this namespace is Clojure analysis with no SonarQube in it:
  a rewrite-clj node stream, rules over that stream, rules over clj-kondo's
  analysis output, and the measures derived from either. Nothing here imports
  org.sonar.

  It exists because source-sensor reached into sixteen namespaces directly.
  That is a shared codebase, not a dependency -- the analysis could not be
  consumed by anything else without dragging the same sixteen requires along,
  and could not be extracted without breaking every one of them. One seam makes
  both possible: a consumer requires `scan`, and the rules behind it can move,
  split or gain siblings without any consumer noticing.

  The rules were already uniform and it was not written down. Six take a node
  stream and are named `findings`; two take clj-kondo's analysis text. That is
  the whole taxonomy, and it is the reason this file is short.

  NOT tools.analyzer, for the reason parse.clj gives: resolving and
  macroexpanding means loading the namespace, and a scanner that evaluates the
  code it measures runs your side effects on the CI box. A REPL-side consumer
  that has already loaded the code is a different context and may rank higher;
  this surface is for the one that has not."
  (:require [au.com.heisenbergtech.scan.access :as access]
            [au.com.heisenbergtech.scan.analysis :as analysis]
            [au.com.heisenbergtech.scan.callgraph :as callgraph]
            [au.com.heisenbergtech.scan.concurrency :as concurrency]
            [au.com.heisenbergtech.scan.dictionary :as dictionary]
            [au.com.heisenbergtech.scan.highlight :as highlight]
            [au.com.heisenbergtech.scan.interop :as interop]
            [au.com.heisenbergtech.scan.metrics :as metrics]
            [au.com.heisenbergtech.scan.parse :as p]
            [au.com.heisenbergtech.scan.regex :as regex]
            [au.com.heisenbergtech.scan.security :as security]
            [au.com.heisenbergtech.scan.tests :as tests]
            [au.com.heisenbergtech.scan.web :as web]))

;; ── structure ──────────────────────────────────────────────────────────────

(defn parse-source
  "Source text -> {:ok? :nodes :error}.

  NOT `parse`. A var named `parse` on this namespace and the child namespace
  au.com.heisenbergtech.scan.parse compile to the SAME JavaScript path, so in
  ClojureScript one silently overwrites the other and every call becomes
  \"parse is not a function\" at runtime. On the JVM they coexist, which is why
  this survived a green suite and a clean compile and only appeared when the
  code was actually run under bun. No var here may share a name with a child
  namespace segment."
  [text]
  (p/parse text))
(defn leaves   "Token nodes, for CPD and highlighting."  [nodes] (p/leaves nodes))
(defn cpd-image "Duplication image for one token."       [text]  (p/cpd-image text))
(defn spans
  "Highlight spans over token nodes. The token CLASS is analysis; mapping it to
  a renderer's palette is the consumer's job, which is why this returns spans
  rather than anything Sonar-shaped."
  [tokens] (highlight/spans tokens))

;; ── measures ───────────────────────────────────────────────────────────────

(defn measures  "Size and complexity."                   [nodes] (metrics/from-nodes nodes))
(defn line-data "Per-line measures."             [nodes truth]   (metrics/line-data nodes truth))
(defn symbols   "Symbol table, from clj-kondo analysis." [text]   (analysis/symbols text))

;; ── rules ──────────────────────────────────────────────────────────────────

(def node-rules
  "Rules over the node stream. Order fixed so output is stable across runs.
  `:tests` is separate rather than listed here because it applies only to test
  sources, and a rule that silently returns nothing on main sources would be
  indistinguishable from one that is broken."
  [security/findings interop/all-findings concurrency/findings
   regex/findings web/findings access/findings])

(defn seeds
  "Taint sources and sinks this file contributes to the interprocedural pass."
  [nodes]
  (security/seeds nodes))

(defn findings
  "Every node-stream finding for one file. `:test?` adds the test-only rules."
  [nodes & {:keys [test?]}]
  (concat (mapcat #(% nodes) node-rules)
          (when test? (tests/findings nodes))))

(defn prose-findings
  "Rules over docstrings and comments, which need clj-kondo's analysis rather
  than the node stream."
  [analysis-text]
  (dictionary/findings analysis-text))

(defn interprocedural
  "Taint paths crossing function boundaries. Silent without seeds, by design:
  the direct findings are unaffected and a partial answer is not an error."
  [analysis-text {:keys [taints reaches] :as seeds}]
  (when-not (or (empty? taints) (empty? reaches))
    (callgraph/findings analysis-text seeds)))
