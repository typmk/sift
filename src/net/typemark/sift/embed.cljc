(ns net.typemark.sift.embed
  #?(:clj (:require [clojure.edn :as edn]
                    [clojure.java.io :as io])))

#?(:clj
   (defmacro load-edn
     [resource-name]
     (let [r (or (io/resource (str "net/typemark/sift/" resource-name))
                 (throw (ex-info (str resource-name " is not on the classpath") {})))]
       `(quote ~(edn/read-string (slurp r))))))
