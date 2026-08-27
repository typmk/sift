(ns com.typemark.sift.typeflow-test
  "Every assertion here was first checked against assay's notes — the JVM
  compiler's own reflection and boxed-math warnings — over three corpora:
  sift itself (boxed 44/45), agentia (reflection 1/1) and lume (boxed
  P 0.87 R 0.98, reflection P 0.58 R 0.96). The fixtures are the shapes
  those runs taught; the numbers are in the README."
  (:require [clojure.test :refer [deftest is testing]]
            [com.typemark.sift.typeflow :as tf]))

(defn- preds [src] (remove #(= :reflection-unwarned (:kind %)) (tf/predictions src "x.clj" :jvm)))
(defn- kinds [src] (mapv :kind (preds src)))
(defn- at [src] (mapv (juxt :kind :column) (preds src)))
(defn- kinds* [src opts] (mapv :kind (remove #(= :reflection-unwarned (:kind %)) (tf/predictions src "x.clj" :jvm opts))))
(defn- at* [src opts] (mapv (juxt :kind :column) (remove #(= :reflection-unwarned (:kind %)) (tf/predictions src "x.clj" :jvm opts))))

(deftest the-file-rule-rides-beside-the-predictions
  (is (some #(= :reflection-unwarned (:kind %)) (tf/predictions "(defn f [s] (.length s))" "x.clj" :jvm)))
  (is (not-any? #(= :reflection-unwarned (:kind %))
                (tf/predictions "(set! *warn-on-reflection* true)\n(defn f [^String s] (.length s))" "x.clj" :jvm)))
  (testing "a static call counts as interop — the old host rule read only the member name and missed every Class/static"
    (is (some #(= :reflection-unwarned (:kind %)) (tf/predictions "(defn f [s] (Integer/parseInt s))" "x.clj" :jvm)))))

(deftest boxed-math-is-an-operand-the-compiler-cannot-unbox
  (testing "unhinted params box; hinted and literal operands do not"
    (is (= [:boxed-math :boxed-math] (kinds "(defn f [a b] (+ (* a b) a))"))
        "the + and the *, as assay reports two")
    (is (= [] (kinds "(defn f [^long a ^long b] (+ (* a b) a))")))
    (is (= [] (kinds "(defn f [] (+ 1 2))"))))
  (testing "a cast or a known core fn is primitive"
    (is (= [] (kinds "(defn f [x] (inc (long x)))")))
    (is (= [] (kinds "(defn f [xs] (inc (count xs)))"))))
  (testing "a let binding carries its init's tag"
    (is (= [] (kinds "(defn f [x] (let [n (long x)] (inc n)))")))
    (is (= [:boxed-math] (kinds "(defn f [x] (let [n (first x)] (inc n)))"))))
  (testing "a host member the table knows returns a primitive"
    (is (= [] (kinds "(defn f [] (quot (System/currentTimeMillis) 1000))"))))
  (testing "bit ops warn on a boxed operand but return long — measured"
    (is (= [[:boxed-math 19]] (at "(defn f [x] (pos? (bit-and (parse-long x) 3)))"))
        "the bit-and boxes; pos? over its long does not"))
  (testing "a #() is a form; math inside it is scored at the compiler's column"
    (is (= [[:boxed-math 20]] (at "(defn f [xs] (map #(inc (:n %)) xs))"))
        "column 20 is the ( after the #, where the compiler points")))

(deftest reflection-is-a-receiver-or-overload-the-compiler-cannot-type
  (is (= [:reflection] (kinds "(defn f [s] (.length s))")))
  (is (= [] (kinds "(defn f [^String s] (.length s))")))
  (testing "a static call or constructor yields a known class; a call on it resolves"
    (is (= [] (kinds "(defn f [] (.digest (java.security.MessageDigest/getInstance \"SHA-256\")))")))
    (is (= [:reflection] (kinds "(defn f [t] (.getBytes (minify t) \"UTF-8\"))"))))
  (testing "catch binds the exception's class"
    (is (= [] (kinds "(defn f [] (try 1 (catch Exception e (.getMessage e))))"))))
  (testing "doto and -> thread the receiver; members judge on it, not their first argument"
    (is (= [] (kinds "(defn f [u] (doto (java.util.Properties.) (.setProperty \"a\" u)))")))
    (is (= [[:reflection 27]] (at "(defn f [cfg u] (doto cfg (.setProperty \"a\" u)))"))
        "at the member form, on the unknown threaded receiver"))
  (testing "an overloaded constructor with a boxed or unknown argument reflects — lume, measured"
    (is (= #{:boxed-math :reflection}
           (set (kinds "(defn f [ttl] (Date. (+ (System/currentTimeMillis) ttl)))"))))
    (is (= [] (kinds "(defn f [^String s] (java.util.UUID/fromString s))"))
        "a static with one overload resolves by name and arity")))

(deftest the-js-host-predicts-nothing-yet
  (is (= [] (tf/predictions "(defn f [x] (.foo x))" "x.cljs" :js))
      "648 predictions against 0 Closure warnings on defnet — see hosts.edn"))

(defn- with [src opts] (kinds* src opts))

(deftest what-lume-taught-once-every-file-was-judged
  ;; bin/assay-notes writes loaded.edn; before it, five "false positives" were
  ;; in test files the compiler never compiled. These are the model errors
  ;; that were left once only judged files counted — lume 106/106, 26/26.
  (testing "only the two-argument comparison is :inline; three arguments is a plain call and never warns"
    (is (= [] (kinds "(defn f [status] (<= 200 status 299))")))
    (is (= [:boxed-math] (kinds "(defn f [lo hi] (<= lo hi))"))))
  (testing "mod has no :inline at all; quot and rem do"
    (is (= [] (kinds "(defn f [x] (mod x 89))")))
    (is (= [:boxed-math] (kinds "(defn f [x] (rem x 89))"))))
  (testing "alength is an int and abs keeps its operand's primitive"
    (is (= [] (kinds "(defn f [^bytes b] (+ (alength b) 12))")))
    (is (= [] (kinds "(defn f [v] (quot (abs (long v)) 100))"))))
  (testing "a library's inc is not clojure.core/inc"
    (is (= [] (kinds "(defn f [r id v] (prometheus/inc r id v))")))
    (is (= [] (kinds "(defn f [r] (clojure.core/inc (long r)))"))))
  (testing "a ^:const def is inlined as its literal — through var-tags over the file's own trees"
    (let [src "(ns m)\n(def ^:const max-bytes 4096)\n(defn f [xs] (<= (count xs) max-bytes))"]
      (is (= {"m/max-bytes" "long"} (tf/var-tags src)))
      (is (= [] (kinds* src {:var-tags (tf/var-tags src)})))
      (is (= [:boxed-math] (kinds src)) "without the tags a bare def is Object")))
  (testing "^:private is not a return tag"
    (is (= {} (tf/var-tags "(ns m)\n(defn ^:private f [x] x)"))))
  (testing "branches that agree carry their tag: if-let and cond rebinding a builder"
    (is (= [] (kinds "(defn f [x y] (let [b (java.util.Properties.) b (if-let [t x] (.setProperty b \"k\" t) b) b (cond y (.remove b y) :else b)] (.size b)))"))))
  (testing "a static field as an argument is known"
    (is (= [] (kinds "(defn f [^String s] (.getBytes s java.nio.charset.StandardCharsets/UTF_8))"))))
  (testing "a constructor is its class, so an overloaded method on it can be judged"
    (is (= [[:reflection 38]]
           (at* "(defn f [^java.io.OutputStream o ev] (.write (java.io.OutputStreamWriter. o \"UTF-8\") ev))"
                {:var-tags {"OutputStreamWriter/.write" {:returns "void" :overloaded #{1 3}}}}))
        "write(String) / write(char[]) / write(int) and ev is untyped — diplomat.clj:401")
    (is (= [] (with "(defn f [^java.io.OutputStream o ^String ev] (.write (java.io.OutputStreamWriter. o \"UTF-8\") ev))"
                    {:var-tags {"OutputStreamWriter/.write" {:returns "void" :overloaded #{1 3}}}}))))
  (testing "a dumped method return on a known receiver is primitive; on an unknown receiver the call reflects and returns Object"
    (is (= [] (with "(defn f [^java.net.HttpURLConnection c] (< (.getResponseCode c) 400))"
                    {:var-tags {"HttpURLConnection/.getResponseCode" "int"}})))
    (is (= [:boxed-math :reflection] (kinds "(defn f [xs] (inc (.indexOf xs \"p\")))"))
        "agentia ledger.clj:296 — .indexOf is in :host-returns, and that only holds when it resolved")
    (is (= [] (kinds "(defn f [^String xs] (inc (.indexOf xs \"p\")))"))))
  (testing "a static call the dump knows returns its primitive"
    (is (= [] (with "(defn f [c] (- (Character/digit ^char c 10) 1))"
                    {:var-tags {"Character/.digit" {:returns "int" :overloaded #{2}}}})))))

(deftest what-the-held-out-corpora-taught
  (testing "a deref is a node: @(d/transact c [[… (+ now ttl)]]) — agentia, three misses under one"
    (is (= [:boxed-math] (kinds "(defn f [c now ttl] @(d/transact c [[:db/add 1 :x (+ now ttl)]]))"))))
  (testing "a reader conditional is read on the host's branch — the catch class in a .cljc"
    (is (= [] (kinds "(defn f [] (try 1 (catch #?(:clj Exception :cljs :default) e (.getMessage e))))")))
    (is (= [] (kinds "(defn f [e] #?(:clj (.getMessage ^Exception e) :cljs (.-message e)))")))
    (is (= [:reflection] (kinds "(defn f [e] #?(:clj (.getMessage e) :cljs (.-message e)))"))))
  (testing "a bare cond-> step is a call on the threaded value, reported at the cond-> form"
    (is (= [[:boxed-math 16]] (at "(defn f [d b?] (cond-> d b? inc))")))
    (is (= [] (kinds "(defn f [^long d b?] (cond-> d b? inc))"))))
  (testing "constructor overload is per arity: one 1-arg ctor resolves, two reflect"
    (is (= [] (with "(defn f [s] (java.net.URL. s))" {:var-tags {"URL/new" {:returns "URL" :overloaded #{2 4}}}})))
    (is (= [:reflection] (with "(defn f [x] (ProcessBuilder. x))" {:var-tags {"ProcessBuilder/new" {:returns "ProcessBuilder" :overloaded #{1}}}})))))
