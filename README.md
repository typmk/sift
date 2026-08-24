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

45 tests, 134 assertions. `sonar-clojure` runs the same files a second time
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
