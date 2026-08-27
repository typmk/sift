(ns com.typemark.sift.baseline
  "Only NEW findings fail: detekt's baseline, for the shape rules.

  Onboarding a codebase that predates a rule produces a defect list nobody
  reads and a rule that gets switched off in week one (DEFNET-48). A
  baseline freezes what the code has today so the gate judges only what a
  change adds — the ratchet defnet already runs on its tool surface and its
  boundary exports, applied to findings.

  A finding's key is [file rule symbol], NOT its line. A rename above it
  must not churn the file, and a second finding of the same rule on the same
  symbol in the same file is the same finding. What the key cannot tell
  apart — two atom-folds both named `acc` in one file — is counted, so a
  third `acc` is new and a second is not.

  The baseline is EDN, one entry per line, sorted: a committed file read in
  diffs, the same discipline as observations.edn. sift never writes it;
  `render` returns the text and the caller decides where it goes."
  (:require [clojure.string :as str]
            #?(:clj [clojure.edn :as edn]
               :cljs [cljs.reader :as edn])))

(defn key-of
  "[file rule symbol] for a finding. `file` is stored as given; a caller
  with absolute paths and a baseline with relative ones should relativise
  first, and `render` says so in its first line."
  [{:keys [file rule symbol]}]
  [file rule (some-> symbol str)])

(defn counts
  "{key n} over findings — how many of each key the code has."
  [findings]
  (frequencies (map key-of findings)))

(defn new-findings
  "The findings not covered by `baseline` ({key n}): each key's first n
  occurrences are known, the rest are new. Order preserved."
  [baseline findings]
  (loop [fs (seq findings) seen {} out []]
    (if-not fs
      out
      (let [f (first fs)
            k (key-of f)
            n (get seen k 0)]
        (recur (next fs)
               (assoc seen k (inc n))
               (if (< n (get baseline k 0)) out (conj out f)))))))

(defn render
  "The baseline as text: a header line, then one `[[file rule symbol] n]`
  per line, sorted, so a diff shows exactly which finding moved."
  [baseline]
  (str ";; sift baseline — findings the code has today; only NEW ones are reported.\n"
       ";; key [file rule symbol] -> count. Paths as the caller gave them.\n"
       (str/join "\n" (map pr-str (sort-by (comp str first) baseline)))
       "\n"))

(defn parse
  "Baseline text -> {key n}. A comment line is skipped; a line that does
  not read is an error, not a silently empty baseline — an empty baseline
  reports everything, which is the failure that looks like success."
  [text]
  (into {}
        (for [line (str/split-lines (or text ""))
              :let [l (str/trim line)]
              :when (and (seq l) (not (str/starts-with? l ";")))]
          (let [[k n] (edn/read-string l)]
            [k n]))))
