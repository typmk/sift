(ns net.typemark.sift.prose
  (:require [clojure.string :as str]))

(def hedges
  ["this function" "this fn" "this method" "is used to" "is responsible for"
   "simply" "basically" "essentially" "in order to" "the purpose of" "helper function"
   "utility function" "as the name suggests"])

(def placeholder-tokens
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
  [w]
  (-> w (str/replace #"(ing|ed|es|s)$" "")))

(defn- same-word?
  [a b]
  (or (= a b)
      (and (>= (min (count a) (count b)) 4)
           (or (str/starts-with? a b) (str/starts-with? b a)))))

(defn- name-tokens [nm]
  (into #{} (map stem) (remove str/blank? (str/split (str/lower-case nm) #"[-_?!*+]+"))))

(defn- content-words [doc]
  (into #{} (comp (remove stop-words) (map stem)) (words doc)))

(defn- quote-re
  [s]
  (str/replace s #"[.*+?^${}()|\[\]\\]" "\\$0"))

(defn- finding [d rule message]
  (merge (select-keys d [:line :column :end-line :end-column])
         {:rule rule :symbol (some-> (:name d) symbol) :message message}))

(defn- var-findings [{:keys [text params] nm :name :as d}]
  (let [dw (content-words text)
        nt (name-tokens (or nm ""))
        low (str/lower-case text)]
    (cond-> []
      (and (seq dw) (seq nt) (every? (fn [w] (some #(same-word? w %) nt)) dw))
      (conj (finding d :doc/restates-name
                     (str "\"" (str/trim text) "\" says only what `" nm "` already says")))

      (some #(str/includes? low %) hedges)
      (conj (finding d :doc/hedge
                     (str "hedge in docstring: \"" (some #(when (str/includes? low %) %) hedges) "\"")))

      (some #(re-find % text) placeholders)
      (conj (finding d :doc/placeholder "docstring is a placeholder"))

      (and (>= (count params) 2)
           (<= (count (words text)) 25)
           (not-any? (fn [p]
                       (some #(re-find (re-pattern (str "(?i)(^|[^a-z0-9])" (quote-re %) "([^a-z0-9]|$)")) text)
                             (cons p (filter #(>= (count %) 3) (str/split p #"[-_]")))))
                     params))
      (conj (finding d :doc/params-unnamed
                     (str (count params) " parameters and the docstring names none of them: "
                          (str/join ", " (sort params))))))))

(defn- ns-findings [{:keys [text] :as d}]
  (when (or (not (string? text)) (< (count (words text)) 4))
    [(finding d :doc/ns-missing
              (if (string? text)
                (str "namespace docstring is " (count (words text)) " words")
                "namespace has no docstring"))]))

(defn findings
  [docs]
  (vec (mapcat #(if (= :ns (:kind %)) (ns-findings %) (when (string? (:text %)) (var-findings %))) docs)))

(defn overlap
  [{:keys [text params body] nm :name}]
  (let [dw (into #{} (comp (mapcat #(str/split % #"-")) (remove str/blank?) (remove stop-words) (map stem))
                 (words text))
        code (into #{} (map stem) (concat body (mapcat #(str/split % #"[-_]") params) (name-tokens (or nm ""))))]
    {:words (count dw)
     :shared (count (filter (fn [w] (some #(same-word? w %) code)) dw))}))

(def ^:private contract
  #"(?i)\breturns?\b|\bnil\b|\bthrow|\bpromise\b|\bdefault\b|\bascending\b|\bdescending\b|\bas an?\b|[~`{}\[\]]")

(defn narrates-body
  [docs min-words min-overlap]
  (for [d docs
        :when (and (= :var (:kind d)) (string? (:text d)) (seq (:body d)))
        :when (not (re-find contract (:text d)))
        :let [{:keys [words shared]} (overlap d)]
        :when (and (>= words min-words) (>= (/ shared words) min-overlap))]
    (finding d :doc/narrates-body
             (str shared " of " words " words in the docstring are names from the code it documents"))))
