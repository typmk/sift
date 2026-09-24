(ns safe-atom-constructor
  "HARD NEGATIVE. Atoms escape in the returned map — a state constructor.")

(defn login-throttle []
  {:attempts (atom {}) :lockouts (atom {})})
