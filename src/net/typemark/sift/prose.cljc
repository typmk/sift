(ns net.typemark.sift.prose
  "Docstrings, judged the way Vale judges prose — a few rule KINDS over
  what clj-kondo's analysis already carries: every var's `doc`, its
  `arglist-strs`, its row span, and every namespace's `doc`. No parsing of
  its own; `dictionary.cljc` reads the same input for the banned-term
  table and this is its sibling.

  The smells are the ones generated docstrings have and hand-written ones
  do not, each Vale's kind in brackets:

    :doc/restates-name   [repetition]  \"Parses the config.\" on parse-config —
                          the doc's content words are the name's tokens.
    :doc/hedge           [existence]   \"This function…\", \"is used to\",
                          \"simply\", \"basically\", \"in order to\".
    :doc/params-unnamed  [sequence]    two or more parameters, a SHORT doc
                          (25 words or fewer), and it names none of them.
                          Measured on a code-graph tool without the length guard: 272
                          hits, every one a long docstring that explains the
                          design and never says `from-name` — not the smell.
    :doc/placeholder     [existence]   TODO / FIXME / XXX / \"write this\".
    :doc/ns-missing      [occurrence]  a namespace with no docstring, or one
                          under four words.

  Each finding lands on the var (or the ns form) at kondo's name position,
  so a consumer can attach it to the definition. Corpus as spec:
  corpus/prose/{docs.clj, docs.analysis.json} — the JSON is what kondo
  emitted over the file beside it, with paths relativised."
  (:require [clojure.string :as str]
            [net.typemark.sift.json :as json]))

(def rules
  {:doc/restates-name  {:evidence :corpus :note "7 on a code-graph tool, all real" :category :documentation
                        :instruction "Say what the function guarantees or returns, not its name again: the input's shape, the edge case, what nil means."}
   :doc/hedge          {:evidence :corpus :category :documentation
                        :instruction "Delete the hedge. A docstring is the contract; 'this function is used to' says nothing the name did not."}
   :doc/params-unnamed {:evidence :corpus :note "272 -> 133 on a code-graph tool after the length guard; the cut ones were read" :category :documentation
                        :instruction "Name each parameter and what it must be; a reader at the call site has the arglist, not the body."}
   :doc/placeholder    {:evidence :corpus :category :documentation
                        :instruction "Write the docstring or remove the placeholder; a TODO docstring reads as documented in every tool."}
   :doc/ns-missing     {:evidence :corpus :category :documentation
                        :instruction "A namespace docstring says what lives here and why it is separate; one sentence is enough."}})

(def hedges
  "Phrases that describe the docstring instead of the function."
  ["this function" "this fn" "this method" "is used to" "is responsible for"
   "simply" "basically" "essentially" "in order to" "the purpose of" "helper function"
   "utility function" "as the name suggests"])

(def placeholder-tokens
  "Tokens that mark a docstring as unwritten."
  ["TODO" "FIXME" "XXX" "write this" "fill in" "WIP"])

(def ^:private placeholders
  (mapv #(re-pattern (str "(?i)(^|[^a-z])" % "([^a-z]|$)")) placeholder-tokens))

(def ^:private stop-words
  #{"a" "an" "the" "of" "to" "and" "or" "for" "in" "on" "with" "from" "by" "is"
    "are" "it" "its" "this" "that" "as" "at" "be" "given" "returns" "return"})

(defn- words [s]
  (->> (str/split (str/lower-case (or s "")) #"[^a-z0-9?!*+-]+")
       (remove str/blank?)))

(defn- stem
  "Cheap: strip a trailing s/es/ing/ed so `parses` meets `parse`."
  [w]
  (-> w (str/replace #"(ing|ed|es|s)$" "")))

(defn- same-word?
  "`pars`/`parse`, `config`/`configs`: equal after stemming, or one a prefix
  of the other at four letters or more."
  [a b]
  (or (= a b)
      (and (>= (min (count a) (count b)) 4)
           (or (str/starts-with? a b) (str/starts-with? b a)))))

(defn- name-tokens [nm]
  (into #{} (map stem) (remove str/blank? (str/split (str/lower-case nm) #"[-_?!*+]+"))))

(defn- content-words [doc]
  (into #{} (comp (remove stop-words) (map stem)) (words doc)))

(defn- param-names
  "Symbols in the first arglist string, destructuring flattened: `[path opts]`
  -> #{path opts}; `[{:keys [a b]} & more]` -> #{a b more}."
  [arglist-strs]
  (into #{}
        (comp (mapcat #(re-seq #"[a-zA-Z][a-zA-Z0-9*+!?<>=-]*" %))
              (remove #{"keys" "as" "or" "strs" "syms"}))
        arglist-strs))

(defn- quote-re
  "Regex-quote a parameter name portably — Pattern/quote is JVM-only."
  [s]
  (str/replace s #"[.*+?^${}()|\[\]\\]" "\\$0"))

(defn- at [m]
  {:line (or (get m "name-row") (get m "row"))
   :col (or (get m "name-col") (get m "col") 1)
   :end-line (or (get m "name-end-row") (get m "end-row"))
   :end-col (or (get m "name-end-col") (get m "end-col") 2)})

(defn- finding [m rule message]
  (merge (at m)
         {:rule rule
          :category (get-in rules [rule :category])
          :instruction (get-in rules [rule :instruction])
          :applicability :unspecified
          :symbol (get m "name")
          :message message}))

(defn- var-findings [v]
  (let [doc (get v "doc")
        nm (get v "name")
        args (param-names (get v "arglist-strs"))]
    (when (string? doc)
      (let [dw (content-words doc)
            nt (name-tokens nm)
            low (str/lower-case doc)]
        (cond-> []
          (and (seq dw) (seq nt) (every? (fn [w] (some #(same-word? w %) nt)) dw))
          (conj (finding v :doc/restates-name
                         (str "\"" (str/trim doc) "\" says only what `" nm "` already says")))

          (some #(str/includes? low %) hedges)
          (conj (finding v :doc/hedge
                         (str "hedge in docstring: \"" (some #(when (str/includes? low %) %) hedges) "\"")))

          (some #(re-find % doc) placeholders)
          (conj (finding v :doc/placeholder "docstring is a placeholder"))

          (and (>= (count args) 2)
               (<= (count (words doc)) 25)
               ;; a parameter counts as named if the doc says it, or any
               ;; three-letter piece of it: `from-name` is named by "name".
               (not-any? (fn [p]
                           (some #(re-find (re-pattern (str "(?i)(^|[^a-z0-9])" (quote-re %) "([^a-z0-9]|$)")) doc)
                                 (cons p (filter #(>= (count %) 3) (str/split p #"[-_]")))))
                         args))
          (conj (finding v :doc/params-unnamed
                         (str (count args) " parameters and the docstring names none of them: "
                              (str/join ", " (sort args))))))))))

(defn- ns-findings [n]
  (let [doc (get n "doc")]
    (when (or (not (string? doc)) (< (count (words doc)) 4))
      [(finding n :doc/ns-missing
                (if (string? doc)
                  (str "namespace docstring is " (count (words doc)) " words")
                  "namespace has no docstring"))])))

(defn findings
  "analysis JSON text -> {filename [finding …]}, the shape `dictionary`
  returns, so the two merge into one prose pass."
  [analysis-text]
  (let [a (get (json/read-str analysis-text) "analysis")
        per-var (for [v (get a "var-definitions" [])
                      f (var-findings v)]
                  [(get v "filename") f])
        per-ns (for [n (get a "namespace-definitions" [])
                     f (ns-findings n)]
                 [(get n "filename") f])]
    (reduce (fn [m [file f]] (update m file (fnil conj []) f))
            {}
            (concat per-ns per-var))))
