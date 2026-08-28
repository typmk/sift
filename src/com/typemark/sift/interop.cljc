(ns com.typemark.sift.interop
  "Security rules for the Java libraries Clojure calls.

  The premise: Clojure runs on the JDK and reaches for the same
  `MessageDigest`, `Cipher`, `SSLContext`, `DocumentBuilderFactory` and
  `ObjectInputStream` as Java does, so it inherits the same CVE classes.
  Sonar's Java analyzer cannot see any of it -- it parses Java source, and
  measured against this server its 720 rules share no repository with Scala's
  41 or Kotlin's 145. Each JVM language pays for its own.

  What it does NOT try to reproduce: the Java rules about Java's syntax --
  null dereference, equals/hashCode, try-with-resources, mutable statics.
  Those are meaningless here. Nor most of Java's concurrency rules, since
  Clojure's defaults are immutable; the concurrency hazards Clojure DOES have
  are its own, and live in com.typemark.sift.concurrency.

  Class names are resolved through the ns form's :import, so both
  `(MessageDigest/getInstance ...)` and
  `(java.security.MessageDigest/getInstance ...)` match the same rule."
  (:require [clojure.string :as str]
            [com.typemark.sift.tree :as tree]))

(defn imports
  "simple class name -> fully qualified, from the ns form's :import clauses.
  Handles both `[java.security MessageDigest Signature]` and a bare
  `java.util.Random`."
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
  "A class name as written -> fully qualified, if we can tell."
  [imported nm]
  (cond
    (nil? nm) nil
    (str/includes? nm ".") nm
    :else (get imported nm)))

(def detections
  "Class, member, and optionally a predicate over the first string argument.
  Nothing else: title, severity, CWE and prose live in
  org/sonar/l10n/clj/rules/clj-kondo/<key>.{json,html}.

  This table STAYS in code, unlike the metadata. The argument predicates are
  regular expressions -- programs, not content -- and moving them to JSON
  would mean either losing reader-syntax validation or inventing a matcher
  DSL to put it back. The metadata moved because a title, a severity and a
  CWE are content: reviewable by someone who does not write Clojure, and
  translatable. A class/member/regex triple is neither.

  Consistency would argue both belong in resources. That is the Occam
  reading. These are two concerns that happen to sit near each other -- WHAT
  to detect, and HOW to describe it -- and they are already correctly
  separated."
  ;; jndi-injection, unsafe-deserialization, predictable-temp-file and
  ;; shell-invocation (ProcessBuilder) are rules.edn's now — data rules with
  ;; :dynamic and :tag guards over typeflow's annotated walk, which is how
  ;; the instance form (.lookup ctx n) became detectable. What stays here is
  ;; what needs the hardening walk.
  [{:key "xml-external-entity" :class "javax.xml.parsers.DocumentBuilderFactory" :member "newInstance"}
   {:key "xml-external-entity" :class "javax.xml.parsers.SAXParserFactory" :member "newInstance"}
   {:key "xml-external-entity" :class "javax.xml.transform.TransformerFactory" :member "newInstance"}
   {:key "xml-external-entity" :class "javax.xml.stream.XMLInputFactory" :member "newInstance"}])

(def ^:private by-target
  (reduce (fn [m r] (update m [(:class r) (:member r)] (fnil conj []) r)) {} detections))

(def ^:private hardening-calls
  "Setter -> the literal that means \"locked down\". Checked as CALLS with
  their argument, never as text in the enclosing form: the previous version
  regex-matched the form's raw source, so a `;; TODO disallow-doctype-decl`
  comment silenced the rule, and so did setting the very same feature to
  false. A comment must never disable a security finding."
  {".setXIncludeAware"          "false"
   ".setExpandEntityReferences" "false"})

(def ^:private hardening-features
  "Feature URI fragment -> the value that hardens it."
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
  "True when this call is a parser lock-down with the right polarity."
  [nodes n]
  (when (= :list (:tag n))
    (let [args (tree/arguments nodes n)
          head (:head n)]
      (cond
        (contains? hardening-calls head)
        (= (get hardening-calls head) (:text (last args)))

        (contains? #{".setFeature" ".setProperty" ".setAttribute"} head)
        (let [[k v] (take-last 2 args)
              key-text (or (tree/unquote-string k) (:text k) "")]
          (boolean (some (fn [[frag want]]
                           (and (str/includes? key-text frag)
                                (or (= want "") (= want (:text v)))))
                         hardening-features)))

        :else false))))

(defn- hardened?
  "True when the enclosing form contains a real hardening CALL."
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
  "A reify/proxy of a TrustManager or HostnameVerifier. There is no safe
  reason to implement these by hand in application code: the JDK's default
  already does the checking, and overriding it exists to switch it off."
  [nodes]
  (for [n (tree/lists-headed-by nodes #{"reify" "proxy"})
        :when (some #(and (= :symbol (:type %)) (contains? trust-types (:text %)))
                    (tree/children-of nodes n))]
    {:rule "trust-all-certificates"
     :line (:line n) :col (:col n) :end-line (:end-line n) :end-col (:end-col n)
     :message "hand-written TrustManager/HostnameVerifier -- confirm it does not accept every certificate"}))

(defn findings
  "Java-interop security findings for one parsed file."
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
