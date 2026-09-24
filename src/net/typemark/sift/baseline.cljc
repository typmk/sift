(ns net.typemark.sift.baseline
  (:require [clojure.string :as str]
            #?(:clj [clojure.edn :as edn]
               :cljs [cljs.reader :as edn])))

(defn key-of
  [{:keys [file rule symbol]}]
  [file rule (some-> symbol str)])

(defn counts
  [findings]
  (frequencies (map key-of findings)))

(defn new-findings
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
  [baseline]
  (str ";; sift baseline — findings the code has today; only NEW ones are reported.\n"
       ";; key [file rule symbol] -> count. Paths as the caller gave them.\n"
       (str/join "\n" (map pr-str (sort-by (comp str first) baseline)))
       "\n"))

(defn parse
  [text]
  (into {}
        (for [line (str/split-lines (or text ""))
              :let [l (str/trim line)]
              :when (and (seq l) (not (str/starts-with? l ";")))]
          (let [[k n] (edn/read-string l)]
            [k n]))))
