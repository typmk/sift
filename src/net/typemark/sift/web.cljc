(ns net.typemark.sift.web
  (:require [clojure.string :as str]
            [net.typemark.sift.tree :as tree]))

(def ^:private raw-html
  #{"raw" "h/raw" "hiccup.util/raw-string" "raw-string" "hiccup.core/raw"
    "hiccup2.core/raw" "util/raw-string"})

(def ^:private state-changing #{":post" ":put" ":patch" ":delete" "POST" "PUT" "PATCH" "DELETE"})

(def ^:private logging
  #{"log/info" "log/warn" "log/error" "log/debug" "log/trace" "println" "prn"
    "timbre/info" "timbre/warn" "timbre/error" "timbre/debug" "tap>"})

(def ^:private secret-name
  #"(?i)(^|[-_*/.])(passwords?|passwds?|secrets?|api[-_]?keys?|tokens?|credentials?|private[-_]?keys?|access[-_]?keys?|client[-_]?secrets?|session[-_]?ids?|jwts?)([-_*?!]|$)")

(defn- direct-text
  [nodes n]
  (let [d (inc (:depth n))]
    (str/join " " (map #(tree/text nodes %) (filter #(= d (:depth %)) (tree/children-of nodes n))))))

(defn xss-unescaped-output
  [nodes]
  (for [l (tree/lists-headed-by nodes raw-html)
        :let [a (tree/first-argument nodes l)]
        :when (and a (not (tree/literal? a)))]
    (tree/hit l (str (:head l) " renders a computed value without escaping it"))))

(defn- route-method?
  [nodes n]
  (and (= :keyword (:type n))
       (contains? state-changing (:text n))
       (let [p (tree/parent nodes n)
             v (when p (tree/parent nodes p))]
         (and p (= :map (:tag p)) v (= :vector (:tag v))
              (= :string (:type (first (remove (fn [k] (= :trivia (:type k)))
                                               (filter (fn [k] (= (inc (:depth v)) (:depth k)))
                                                       (tree/children-of nodes v))))))))))

(defn csrf-protection-absent
  [nodes]
  (let [web-ns? (some (fn [n] (and (= :symbol (:type n))
                                   (re-find #"(?i)(^|[./])(ring|reitit|compojure|muuntaja|handler|routes|middleware)([./]|$)"
                                            (:text n))))
                      nodes)
        all (str/join " " (map :text (filter #(= :keyword (:type %)) nodes)))
        methods (and web-ns? (some #(str/includes? all %) state-changing))
        guarded (some (fn [n] (and (= :symbol (:type n))
                                   (re-find #"(?i)anti-forgery|csrf" (:text n))))
                      nodes)]
    (when (and methods (not guarded))
      (for [n (take 1 (filter #(route-method? nodes %) nodes))]
        (tree/hit n "state-changing routes here, and no anti-forgery middleware named in this namespace")))))

(defn sensitive-data-logged
  [nodes]
  (for [l (tree/lists-headed-by nodes logging)
        a (tree/arguments nodes l)
        :when (and (= :symbol (:type a)) (re-find secret-name (:text a)))]
    (tree/hit a (str "'" (:text a) "' is credential-shaped and is being logged"))))

(defn cookie-missing-security-flags
  [nodes]
  (let [cookie-ctx (or (some #(and (= :keyword (:type %))
                                   (contains? #{":cookies" ":set-cookie" ":session-cookie-attrs"}
                                              (:text %)))
                             nodes)
                       (some #(and (contains? tree/call-tags (:tag %))
                                   (re-find #"(?i)set-cookie|wrap-session|wrap-cookies"
                                            (or (:head %) "")))
                             nodes))]
    (when cookie-ctx
      (for [n nodes
            :when (and (= :map (:tag n)) (not (:commented? n)) (not (:quoted? n)))
            :let [t (direct-text nodes n)]
            :when (re-find #":value|:max-age|:expires" t)
            :when (or (not (re-find #":http-only\s+true" t))
                      (not (re-find #":secure\s+true" t))
                      (re-find #":secure\s+false|:http-only\s+false" t))]
        (tree/hit n "cookie without :http-only true and :secure true")))))
