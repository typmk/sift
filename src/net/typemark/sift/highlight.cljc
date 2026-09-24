(ns net.typemark.sift.highlight
  (:require [clojure.string :as str]
            [net.typemark.sift.forms :as forms]))

(defn- constant-name?
  [text]
  (or (and (str/starts-with? text "*") (str/ends-with? text "*"))
      (and (> (count text) 1)
           (= text (str/upper-case text))
           (re-find #"[A-Z]" text))))

(defn type-of
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
  [tokens]
  (into []
        (keep (fn [t]
                (when-let [ty (type-of t)]
                  {:line (:line t) :col (:col t)
                   :end-line (:end-line t) :end-col (:end-col t)
                   :type ty})))
        tokens))
