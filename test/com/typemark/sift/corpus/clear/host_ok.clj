(ns host-ok
  (:import [java.util ArrayList]))

(set! *warn-on-reflection* true)

(defn parse-port [s]
  (try (Integer/parseInt s)
       (catch NumberFormatException e
         (throw (ex-info "not a port" {:s s} e)))))

(defn names []
  (vec (ArrayList.)))

(defn size [^ArrayList xs]
  (.size xs))
