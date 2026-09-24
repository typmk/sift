(ns net.typemark.sift.highlight
  "Maps token classes to Sonar's highlighting vocabulary.

  Sonar's palette was drawn for curly-brace languages, so the mapping is a
  judgement, not a translation: a Clojure keyword is a literal, and the
  special forms are the nearest thing the language has to reserved words."
  (:require [clojure.string :as str]
            [net.typemark.sift.forms :as forms]))

(defn- constant-name?
  "Earmuffed dynamic vars and SCREAMING names read as constants."
  [text]
  (or (and (str/starts-with? text "*") (str/ends-with? text "*"))
      (and (> (count text) 1)
           (= text (str/upper-case text))
           (re-find #"[A-Z]" text))))

(defn type-of
  "Token -> Sonar TypeOfText name, or nil for tokens left unstyled.
  Names, not enum values, so this namespace stays free of the Sonar API and
  can be tested without it."
  [{:keys [type text]}]
  (case type
    :comment "COMMENT"
    (:string :regex :char) "STRING"
    (:number :keyword) "CONSTANT"
    :symbol (cond
              (contains? forms/special text) "KEYWORD"
              (constant-name? text) "CONSTANT"
              :else nil)
    nil))

(defn spans
  "Tokens -> [{:line :col :end-line :end-col :type}] for those worth styling."
  [tokens]
  (into []
        (keep (fn [t]
                (when-let [ty (type-of t)]
                  {:line (:line t) :col (:col t)
                   :end-line (:end-line t) :end-col (:end-col t)
                   :type ty})))
        tokens))
