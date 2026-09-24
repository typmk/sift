(ns net.typemark.sift.interop
  (:require [clojure.string :as str]
            [net.typemark.sift.tree :as tree]))

(defn imports
  [nodes]
  (let [import-forms (tree/lists-headed-by nodes #{":import"})]
    (reduce
     (fn [acc form]
       (let [kids (remove #(= :trivia (:type %)) (tree/children-of nodes form))]
         (reduce
          (fn [a n]
            (cond
              (= :vector (:tag n))
              (let [syms (->> (tree/children-of nodes n)
                              (filter #(= :symbol (:type %)))
                              (map :text))]
                (if-let [pkg (first syms)]
                  (reduce (fn [m c] (assoc m c (str pkg "." c))) a (rest syms))
                  a))

              (and (= :symbol (:type n)) (str/includes? (:text n) "."))
              (assoc a (last (str/split (:text n) #"\.")) (:text n))

              :else a))
          acc kids)))
     {} import-forms)))

(defn- resolve-class
  [imported nm]
  (cond
    (nil? nm) nil
    (str/includes? nm ".") nm
    :else (get imported nm)))

(def detections
  [{:key "xml-external-entity" :class "javax.xml.parsers.DocumentBuilderFactory" :member "newInstance"}
   {:key "xml-external-entity" :class "javax.xml.parsers.SAXParserFactory" :member "newInstance"}
   {:key "xml-external-entity" :class "javax.xml.transform.TransformerFactory" :member "newInstance"}
   {:key "xml-external-entity" :class "javax.xml.stream.XMLInputFactory" :member "newInstance"}])

(def ^:private by-target
  (reduce (fn [m r] (update m [(:class r) (:member r)] (fnil conj []) r)) {} detections))

(def ^:private hardening-calls
  {".setXIncludeAware"          "false"
   ".setExpandEntityReferences" "false"})

(def ^:private hardening-features
  {"disallow-doctype-decl"        "true"
   "FEATURE_SECURE_PROCESSING"    "true"
   "external-general-entities"    "false"
   "external-parameter-entities"  "false"
   "load-external-dtd"            "false"
   "ACCESS_EXTERNAL_DTD"          ""
   "ACCESS_EXTERNAL_STYLESHEET"   ""
   "SUPPORT_DTD"                  "false"})

(defn- enclosing-top-level [nodes n]
  (->> nodes
       (filter #(and (= :list (:tag %))
                     (<= (:line %) (:line n))
                     (>= (:end-line %) (:end-line n))))
       (sort-by :depth)
       first))

(defn- hardening-call?
  [nodes n]
  (when (= :list (:tag n))
    (let [args (tree/arguments nodes n)
          head (:head n)]
      (cond
        (contains? hardening-calls head)
        (= (get hardening-calls head) (:text (last args)))

        (contains? #{".setFeature" ".setProperty" ".setAttribute"} head)
        (let [[k v] (take-last 2 args)
              key-text (or (tree/unquote-string k) (tree/text nodes k) "")]
          (boolean (some (fn [[frag want]]
                           (and (str/includes? key-text frag)
                                (or (= want "") (= want (:text v)))))
                         hardening-features)))

        :else false))))

(defn- hardened?
  [nodes n]
  (when-let [form (enclosing-top-level nodes n)]
    (boolean (some #(hardening-call? nodes %) (tree/children-of nodes form)))))

(defn- matches? [nodes lst {:keys [arg dynamic]}]
  (and (if dynamic
         (let [a (tree/first-argument nodes lst)] (and a (not (tree/literal? a))))
         true)
       (or (nil? arg)
           (when-let [s (tree/unquote-string (tree/first-argument nodes lst))]
             (boolean (re-find arg s))))))

(def ^:private trust-types
  #{"X509TrustManager" "TrustManager" "X509ExtendedTrustManager" "HostnameVerifier"
    "javax.net.ssl.X509TrustManager" "javax.net.ssl.TrustManager"
    "javax.net.ssl.X509ExtendedTrustManager" "javax.net.ssl.HostnameVerifier"})

(defn- trust-all-findings
  [nodes]
  (for [n (tree/lists-headed-by nodes #{"reify" "proxy"})
        :when (some #(and (= :symbol (:type %)) (contains? trust-types (:text %)))
                    (tree/children-of nodes n))]
    {:rule "trust-all-certificates"
     :line (:line n) :col (:col n) :end-line (:end-line n) :end-col (:end-col n)
     :message "hand-written TrustManager/HostnameVerifier -- confirm it does not accept every certificate"}))

(defn findings
  [nodes]
  (let [imported (imports nodes)]
    (for [n nodes
          :when (and (= :list (:tag n)) (not (:commented? n)) (:head n))
          :let [[cls member] (tree/head-parts (:head n))
                fq (resolve-class imported cls)]
          :when fq
          r (distinct (concat (get by-target [fq member])
                              (when-not (= :new member) (get by-target [fq :new]))))
          :when (and (or (= (:member r) member)
                         (and (= :new (:member r)) (= :new member)))
                     (matches? nodes n r)
                     (not (and (= "xml-external-entity" (:key r))
                               (hardened? nodes n))))]
      {:rule (:key r)
       :line (:line n) :col (:col n)
       :end-line (:end-line n) :end-col (:end-col n)
       :message (str fq (when (string? member) (str "/" member))
                     (if (= "xml-external-entity" (:key r))
                       " -- confirm external entity resolution is disabled"
                       " -- see the rule description"))})))

(defn all-findings [nodes]
  (concat (findings nodes) (trust-all-findings nodes)))
