(ns net.typemark.sift.parity
  (:require [net.typemark.sift :as sift]
            [net.typemark.sift.registry :as registry]
            [clojure.string :as str]
            #?(:cljs ["fs" :as fs])
            #?(:cljs ["path" :as path])))

(def root "test/net/typemark/sift/corpus")

(defn- files []
  #?(:clj  (->> (file-seq (java.io.File. root))
                (filter #(re-find #"\.clj[cs]?$" (.getName ^java.io.File %)))
                (map #(.getPath ^java.io.File %)))
     :cljs (letfn [(walk [d] (mapcat (fn [e]
                                       (let [p (path/join d e)]
                                         (if (.isDirectory (fs/statSync p)) (walk p) [p])))
                                     (fs/readdirSync d)))]
             (filter #(re-find #"\.clj[cs]?$" %) (walk root)))))

(defn- slurp* [p] #?(:clj (slurp p) :cljs (str (fs/readFileSync p "utf8"))))

(defn- flatten-units [us] (mapcat #(cons % (flatten-units (:children %))) us))

(def ^:private config
  {:rulesets (set (keys registry/rulesets))
   :rules {:doc/ns-missing :info :doc/docstring :info :doc/comment :info
           :tenancy/ambiguous-owner-check {:tenant-pattern "owner|tenant"}
           :tenancy/unscoped-tenant-query {:tenant-pattern "owner|tenant"}}})

(defn rows []
  (let [inputs (for [p (files)] {:path p :text (slurp* p)})
        {:keys [findings errors]} (sift/lint (sift/linter config) inputs)]
    (sort
     (concat (for [f findings] [(:file f) (str (:rule f)) (:line f) (:column f)])
             (for [e errors] [(:file e) "error"])
             (for [{:keys [path text]} inputs
                   u (flatten-units (sift/units text path))]
               [path (str (:name u)) (:cognitive u) (:cyclomatic u)])))))

(defn -main [& _]
  (doseq [r (rows)] (println (pr-str r))))

#?(:cljs (set! *main-cli-fn* -main))
