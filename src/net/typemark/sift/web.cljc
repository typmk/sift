(ns net.typemark.sift.web
  "Rules for the Clojure web stack -- hiccup, ring, reitit.

  This is the bucket Java fills with 103 Spring and JavaEE rules. The
  framework differs; the vulnerability classes do not. XSS (CWE-79), CSRF
  (CWE-352) and credential exposure (CWE-200, CWE-522) were the largest
  remaining gaps against Java's coverage, and all three live in framework
  usage rather than in the language.

  No generic analyzer ships these for Clojure, because they require knowing
  what hiccup's escaping guarantees are and where ring's middleware sits."
  (:require [clojure.string :as str]
            [net.typemark.sift.tree :as tree]))

(def ^:private raw-html
  "Ways to put a string into a page without hiccup escaping it. Each exists
  precisely to bypass the escaping, which is why each needs a reason."
  #{"raw" "h/raw" "hiccup.util/raw-string" "raw-string" "hiccup.core/raw"
    "hiccup2.core/raw" "util/raw-string"})

(def ^:private state-changing #{":post" ":put" ":patch" ":delete" "POST" "PUT" "PATCH" "DELETE"})

(def ^:private logging
  #{"log/info" "log/warn" "log/error" "log/debug" "log/trace" "println" "prn"
    "timbre/info" "timbre/warn" "timbre/error" "timbre/debug" "tap>"})

(def ^:private secret-name
  #"(?i)(^|[-_*/.])(passwords?|passwds?|secrets?|api[-_]?keys?|tokens?|credentials?|private[-_]?keys?|access[-_]?keys?|client[-_]?secrets?|session[-_]?ids?|jwts?)([-_*?!]|$)")

(defn- direct-text
  "The text of a form's DIRECT children only. Using the whole subtree made an
  outer map inherit its inner map's keys, so a cookie map reported twice."
  [nodes n]
  (let [d (inc (:depth n))]
    (str/join " " (map #(tree/text nodes %) (filter #(= d (:depth %)) (tree/children-of nodes n))))))

(defn findings [nodes]
  (concat
   (for [l (tree/lists-headed-by nodes raw-html)
         :let [a (tree/first-argument nodes l)]
         :when (and a (not (tree/literal? a)))]
     {:rule "xss-unescaped-output"
      :line (:line l) :col (:col l) :end-line (:end-line l) :end-col (:end-col l)
      :message (str (:head l) " renders a computed value without escaping it")})

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
       ;; a route METHOD is a map key in route DATA — ["/pay" {:post h}]:
       ;; the map's parent is a vector whose first element is the path.
       ;; `:delete` as an argument to (crud-decision … :delete id) is an
       ;; operation and {:delete (partial delete-op …)} a dispatch table;
       ;; both fired on a production service, neither is a route. reitit takes a bare
       ;; handler as the value, so the value's shape cannot decide it.
       (for [n (take 1 (filter #(and (= :keyword (:type %))
                                     (contains? state-changing (:text %))
                                     (let [p (tree/parent nodes %)
                                           v (when p (tree/parent nodes p))]
                                       (and p (= :map (:tag p)) v (= :vector (:tag v))
                                            (= :string (:type (first (remove (fn [k] (= :trivia (:type k)))
                                                                             (filter (fn [k] (= (inc (:depth v)) (:depth k)))
                                                                                     (tree/children-of nodes v)))))))))
                               nodes))]
         {:rule "csrf-protection-absent"
          :line (:line n) :col (:col n) :end-line (:end-line n) :end-col (:end-col n)
          :message "state-changing routes here, and no anti-forgery middleware named in this namespace"})))

   (for [l (tree/lists-headed-by nodes logging)
         :let [args (tree/arguments nodes l)]
         a args
         :when (and (= :symbol (:type a)) (re-find secret-name (:text a)))]
     {:rule "sensitive-data-logged"
      :line (:line a) :col (:col a) :end-line (:end-line a) :end-col (:end-col a)
      :message (str "'" (:text a) "' is credential-shaped and is being logged")})

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
         {:rule "cookie-missing-security-flags"
          :line (:line n) :col (:col n) :end-line (:end-line n) :end-col (:end-col n)
          :message "cookie without :http-only true and :secure true"})))))
