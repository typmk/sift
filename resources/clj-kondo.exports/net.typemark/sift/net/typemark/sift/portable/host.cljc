(ns net.typemark.sift.portable.host)

(def ^:private catch-all-classes
  #{"Exception" "Throwable" "java.lang.Exception" "java.lang.Throwable"
    ":default" "js/Error" "js/Object" "Object"})

(defn- constant?
  [forms]
  (or (empty? forms)
      (and (= 1 (count forms))
           (let [f (first forms)]
             (or (nil? f) (keyword? f) (string? f) (number? f) (boolean? f)
                 (and (vector? f) (empty? f)) (and (map? f) (empty? f)))))))

(defn catch-all-swallow
  [form]
  (when (and (seq? form) (= 'catch (first form)))
    (let [[_ cls _ & body] form
          cls-text (when (or (symbol? cls) (keyword? cls)) (str cls))]
      (when (and cls-text (contains? catch-all-classes cls-text) (constant? body))
        {:symbol (symbol cls-text)
         :message (str "catch " cls-text " returns a constant; the host's failure is now a value that looks like success")
         :applicability :unspecified}))))
