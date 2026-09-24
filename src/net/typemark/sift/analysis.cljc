(ns net.typemark.sift.analysis
  (:require [net.typemark.sift.json :as json]))

(defn- span [m rk ck erk eck]
  (when (and (get m rk) (get m ck) (get m erk) (get m eck))
    {:line (get m rk) :col (get m ck)
     :end-line (get m erk) :end-col (get m eck)}))

(defn- name-span [m]
  (or (span m "name-row" "name-col" "name-end-row" "name-end-col")
      (span m "row" "col" "end-row" "end-col")))

(defn symbols
  [analysis-text]
  (let [a       (get (json/read-str analysis-text) "analysis")
        defs    (get a "var-definitions" [])
        usages  (get a "var-usages" [])
        locals  (get a "locals" [])
        lusages (get a "local-usages" [])

        local-refs (reduce (fn [m u]
                             (if-let [s (name-span u)]
                               (update m [(get u "filename") (get u "id")] (fnil conj []) s)
                               m))
                           {} lusages)

        var-refs (reduce (fn [m u]
                           (if-let [s (name-span u)]
                             (update m [(get u "filename") (get u "to") (get u "name")]
                                     (fnil conj []) s)
                             m))
                         {} usages)

        local-syms (for [l locals
                         :let [f (get l "filename")
                               d (span l "row" "col" "end-row" "end-col")]
                         :when d]
                     [f {:declaration d
                         :references (get local-refs [f (get l "id")] [])}])

        var-syms (for [d defs
                       :let [f (get d "filename")
                             s (name-span d)]
                       :when s]
                   [f {:declaration s
                       :references (get var-refs [f (get d "ns") (get d "name")] [])}])]
    (reduce (fn [m [f sym]] (update m f (fnil conj []) sym))
            {}
            (concat local-syms var-syms))))
