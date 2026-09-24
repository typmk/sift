(ns net.typemark.sift.json
  #?(:bb  (:require [cheshire.core :as json])
     :clj (:require [clojure.data.json :as json])))

(defn read-str [s]
  #?(:bb   (json/parse-string s)
     :clj  (json/read-str s)
     :cljs (js->clj (js/JSON.parse s))))
