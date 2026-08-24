(ns au.com.heisenbergtech.scan.json
  "JSON reading, per platform.

  The only thing in this library that was not already portable. Everything
  else -- including the rules ABOUT java.beans.XMLDecoder, ProcessBuilder and
  clojure.java.shell -- names those as data to be found in the code under
  analysis, never as a dependency of the analyser. The library requires
  clojure.string, its own namespaces, and this.

  String keys on both platforms, because the callers index clj-kondo's output
  by its own key names and a keywordising reader would silently return nil for
  every one of them."
  #?(:clj (:require [clojure.data.json :as json])))

(defn read-str [s]
  #?(:clj  (json/read-str s)
     :cljs (js->clj (js/JSON.parse s))))
