(ns data-rules)

;; only what splint does not say — see rules.edn's header
(defn g [xs] (first (filter even? xs)))
(defn j [] (Thread/sleep 100))
(defn m [a x] (swap! a (fn [v] (conj v (count @a) x))))

;; host interop as data — full class names resolve without an import
(defn r [in] (java.io.ObjectInputStream. in))
(defn t [] (java.io.File/createTempFile "a" "b"))
(defn p [c] (java.lang.ProcessBuilder. c))
(defn look [n] (javax.naming.InitialContext/doLookup n))
