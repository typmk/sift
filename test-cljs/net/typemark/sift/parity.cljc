(ns net.typemark.sift.parity
  (:require [net.typemark.sift :as sift]
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

(defn rows []
  (sort
   (for [p (files)
         :let [{:keys [findings units]} (sift/analyze {:text (slurp* p) :path p})]
         row (concat (for [f findings] [p (str (:rule f)) (:line f)])
                     (for [u (flatten-units units)] [p (str (:name u)) (:cognitive u) (:cyclomatic u)]))]
     row)))

(defn -main [& _]
  (doseq [r (rows)] (println (pr-str r))))

#?(:cljs (set! *main-cli-fn* -main))
