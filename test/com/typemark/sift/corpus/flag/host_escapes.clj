(ns host-escapes
  (:import [java.util ArrayList HashMap]))

(defn parse-port [s]
  (try (Integer/parseInt s)
       (catch Exception _ nil)))

(defn read-config [f]
  (try (slurp f)
       (catch Throwable _ :failed)))

(defn handled [f]
  (try (slurp f)
       (catch Exception e (throw (ex-info "config" {:file f} e)))))

(defn as-data [f]
  (try (slurp f)
       (catch Exception e {:error (.getMessage e)})))

(defn names []
  (ArrayList.))

(defn index []
  (HashMap. 16))

(defn ok-vec []
  (vec (ArrayList.)))
