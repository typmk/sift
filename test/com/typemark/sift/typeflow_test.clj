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
(defn- with [src opts] (kinds* src opts))

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
  (testing "a host member the oracle knows returns a primitive; without the oracle it is known, unnamed, and boxes"
    (is (= [] (with "(defn f [] (quot (System/currentTimeMillis) 1000))"
                    {:classes {:classes {"java.lang.System" {:methods {"currentTimeMillis" [{:params [] :returns "long" :static? true}]}}} :by-simple {"System" ["java.lang.System"]}}})))
    (is (= [:boxed-math] (kinds "(defn f [] (quot (System/currentTimeMillis) 1000))"))))
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
  (testing "an overloaded constructor with a boxed argument reflects — judged by the oracle's table, never by a hand list"
    (is (= #{:boxed-math :reflection}
           (set (with "(ns x (:import [java.util Date]))\n(defn f [ttl] (Date. (+ (System/currentTimeMillis) ttl)))"
                      {:classes {:classes {"java.util.Date" {:supers ["Object"] :ctors [{:params ["long"]} {:params ["String"]}] :methods {}}
                                           "java.lang.System" {:methods {"currentTimeMillis" [{:params [] :returns "long" :static? true}]}}}
                                 :by-simple {"Date" ["java.util.Date"] "System" ["java.lang.System"]}}}))))
    (is (= [:boxed-math] (kinds "(defn f [ttl] (Date. (+ (System/currentTimeMillis) ttl)))"))
        "without the table: the boxing is text, the constructor has no verdict")
    (is (= [] (kinds "(defn f [^String s] (java.util.UUID/fromString s))"))
        "an undumped static is known, unnamed")))

(deftest the-js-host-predicts-without-an-externs-set-too
  ;; the 648-vs-0 that once switched this host off was an empty oracle judging
  ;; a model; with no externs set every non-js member access is reported
  (is (= [:uninferred] (mapv :kind (remove #(= :reflection-unwarned (:kind %)) (tf/predictions "(defn f [x] (.foo x))" "x.cljs" :js))))))

(def classes
  "A slice of what bin/oracle dumps, enough for the assertions below —
  supers, constructors and methods with parameter types."
  {"OutputStreamWriter" {:supers ["Writer" "Object"] :ctors [{:params ["OutputStream"]} {:params ["OutputStream" "String"]} {:params ["OutputStream" "Charset"]}]
                         :methods {"write" [{:params ["String"] :returns "void"} {:params ["char[]"] :returns "void"} {:params ["int"] :returns "void"}
                                            {:params ["String" "int" "int"] :returns "void"}]}}
   "HttpURLConnection" {:supers ["URLConnection" "Object"] :ctors [] :methods {"getResponseCode" [{:params [] :returns "int"}]}}
   "Character" {:supers ["Object"] :ctors [] :methods {"digit" [{:params ["char" "int"] :returns "int" :static? true} {:params ["int" "int"] :returns "int" :static? true}]}}
   "URL" {:supers ["Object"] :ctors [{:params ["String"]} {:params ["URL" "String"]} {:params ["String" "String" "int" "String"]}] :methods {}}
   "ProcessBuilder" {:supers ["Object"] :ctors [{:params ["List"]} {:params ["String[]"]}] :methods {"start" [{:params [] :returns "Process"}]}}
   "Builder" {:supers ["Object"] :ctors [] :methods {"temperature" [{:params ["Double"] :returns "Builder"}]
                                                      ;; declared only on a package-private base: Reflector.getAsMethodOfPublicBase finds nothing
                                                      "maxRetries" [{:params ["Integer"] :returns "Builder" :public? false}]
                                                      "modelName" [{:params ["String"] :returns "Builder"}]
                                                      "anyOf" [{:params ["List"] :returns "Builder"} {:params ["JsonSchemaElement[]"] :returns "Builder"}]
                                                      "build" [{:params [] :returns "Model"}]}}
   "Model" {:supers ["Object"] :ctors [] :methods {"builder" [{:params [] :returns "Builder" :static? true}]}}
   "ToolExecutor" {:supers ["Object"] :ctors [] :methods {"execute" [{:params ["ToolExecutionRequest" "Object"] :returns "String"}]}}
   "ToolExecutionRequest" {:supers ["Object"] :ctors [] :methods {"name" [{:params [] :returns "String"}]}}
   "PersistentVector" {:supers ["List" "IPersistentVector" "Object"]}
   "IPersistentVector" {:supers ["Sequential" "Object"]}
   "String" {:supers ["CharSequence" "Object"] :methods {"indexOf" [{:params ["String"] :returns "int"} {:params ["int"] :returns "int"}]}}})
(def dump
  "The tags.edn shape: classes by full name, reached by simple name."
  {:classes (into {} (map (fn [[k v]] [(str "x." k) v])) classes)
   :by-simple (into {} (map (fn [[k _]] [k [(str "x." k)]])) classes)})
(defn- judged [src] (kinds* src {:classes dump}))

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
  (testing "a ^:const def is inlined as its literal — the oracle's :vars carries its class"
    (let [src "(ns m)\n(def ^:const max-bytes 4096)\n(defn f [xs] (<= (count xs) max-bytes))"]
      (is (= [] (kinds* src {:var-tags {"m/max-bytes" "long"}})))
      (is (= [:boxed-math] (kinds src)) "without the oracle a bare def is Object")))
  (testing "branches that agree carry their tag: if-let and cond rebinding a builder"
    (is (= [] (kinds "(defn f [x y] (let [b (java.util.Properties.) b (if-let [t x] (.setProperty b \"k\" t) b) b (cond y (.remove b y) :else b)] (.size b)))"))))
  (testing "a static field as an argument is known"
    (is (= [] (kinds "(defn f [^String s] (.getBytes s java.nio.charset.StandardCharsets/UTF_8))"))))
  (testing "a constructor is its class, so an overloaded method on it can be judged"
    (is (= [[:reflection 38]]
           (at* "(defn f [^java.io.OutputStream o ev] (.write (java.io.OutputStreamWriter. o \"UTF-8\") ev))"
                {:classes dump}))
        "write(String) / write(char[]) / write(int) and ev is untyped — diplomat.clj:401")
    (is (= [] (judged "(defn f [^java.io.OutputStream o ^String ev] (.write (java.io.OutputStreamWriter. o \"UTF-8\") ev))"))))
  (testing "a dumped method return on a known receiver is primitive; on an unknown receiver the call reflects and returns Object"
    (is (= [] (judged "(defn f [^java.net.HttpURLConnection c] (< (.getResponseCode c) 400))")))
    (is (= [:boxed-math :reflection] (kinds "(defn f [xs] (inc (.indexOf xs \"p\")))"))
        "agentia ledger.clj:296 — .indexOf is in :host-returns, and that only holds when it resolved")
    (is (= [] (judged "(defn f [^String xs] (inc (.indexOf xs \"p\")))")))
    (is (= [:boxed-math] (kinds "(defn f [^String xs] (inc (.indexOf xs \"p\")))")) "without the oracle the call is known, unnamed, and the inc boxes"))
  (testing "a static call the dump knows returns its primitive"
    (is (= [] (judged "(defn f [c] (- (Character/digit ^char c 10) 1))")))))

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
  (testing "one constructor at the arity is taken without looking at the argument; two, and an untyped argument is Object, which fits neither"
    (is (= [] (judged "(defn f [^String s] (java.net.URL. s))")))
    (is (= [] (judged "(defn f [s] (java.net.URL. s))")) "URL(String) is alone at arity 1 — a cast is emitted, no reflection")
    (is (= [:reflection] (judged "(defn f [x] (ProcessBuilder. x))")) "ProcessBuilder(List) and (String[]) — Object fits neither")))

(deftest what-a-corpus-never-learned-from-taught
  ;; clojure-mcp, blind: boxed 79/79, reflection P 0.92 R 0.75 before these.
  (testing "one method at the arity resolves whatever the argument — the theory that (double x) reflects against a Double parameter was wrong, and the dump said so"
    (is (= [] (judged "(defn f [^Builder b x] (.temperature b (double x)))")))
    (is (= [] (judged "(defn f [^Builder b ^String m] (.modelName b m))"))))
  (testing "a method whose only declarer is a non-public class reflects however it matched — (.maxRetries (int 3)) on a langchain4j builder"
    (is (= [:reflection] (judged "(defn f [^Builder b] (.maxRetries b (int 3)))"))))
  (testing "a constant vector is a PersistentVector, a List; a vector with a symbol in it is an IPersistentVector, which is not"
    (is (= [] (judged "(defn f [] (ProcessBuilder. [\"sh\" \"-c\" \"ls\"]))")))
    (is (= [:reflection] (judged "(defn f [repo] (ProcessBuilder. [\"gh\" repo]))")))
    (is (= [:reflection] (judged "(defn f [^Builder b xs] (.anyOf b (vec xs)))")) "anyOf(List) / anyOf(array): (vec …) is an IPersistentVector, neither"))
  (testing "a reflective step in a chain makes every later step reflect; a resolved one carries its return"
    (is (= [:reflection :reflection] (judged "(defn f [xs] (-> (Model/builder) (.anyOf (vec xs)) (.build)))")))
    (is (= [] (judged "(defn f [^String m] (-> (Model/builder) (.modelName m) (.build)))")))
    (is (= [] (judged "(defn f [^String m x] (cond-> (Model/builder) x (.modelName m) :always (.build)))"))
        "cond-> threads the builder into its member steps, so a local named like the step's argument is not the receiver"))
  (testing "doseq and for bind the element, which the compiler never types; :let inside them binds as let"
    (is (= [:reflection] (judged "(defn f [^java.io.File d] (doseq [x (.listFiles d)] (.isFile x)))")))
    (is (= [] (kinds* "(defn f [ps] (for [p ps :let [f (clojure.java.io/file p)] :when (.exists f)] f))"
                      {:var-tags {"clojure.java.io/file" "java.io.File"}}))))
  (testing "a reify method's parameters are typed by the interface"
    (is (= [] (judged "(defn f [] (reify ToolExecutor (execute [_ request _] (.name request))))")))
    (is (= [:reflection] (judged "(defn f [] (reify Unknown (execute [_ request _] (.name request))))"))))
  (testing "char-array and friends are arrays"
    (is (= [] (judged "(defn f [^java.io.Reader r] (.read r (char-array 1024)))")))))

(deftest a-simple-name-with-two-classes-behind-it-is-the-file-s-import
  ;; lume imports java.util.Date and java.sql.Date; keyed by simple name the
  ;; dump kept sql's two constructors and (Date. (+ …)) resolved where the
  ;; compiler, seeing util's Date(long) and Date(String), reflected.
  (let [two {:classes {"java.util.Date" {:supers ["Object"] :ctors [{:params ["long"]} {:params ["String"]}] :methods {}}
                       "java.sql.Date" {:supers ["Object"] :ctors [{:params ["long"]}] :methods {}}}
             :by-simple {"Date" ["java.util.Date" "java.sql.Date"]}}]
    (is (= #{:boxed-math :reflection}
           (set (kinds* "(ns x (:import [java.util Date]))\n(defn f [ttl] (Date. (+ (System/currentTimeMillis) ttl)))" {:classes two})))
        "util's Date: two 1-arg constructors, a boxed Number fits neither")
    (is (= [:boxed-math]
           (kinds* "(ns x (:import [java.sql Date]))\n(defn f [ttl] (Date. (+ (System/currentTimeMillis) ttl)))" {:classes two}))
        "sql's Date: one 1-arg constructor, taken without looking")
    (is (= [:boxed-math]
           (kinds* "(defn f [ttl] (Date. (+ (System/currentTimeMillis) ttl)))" {:classes two}))
        "no import to decide by: no verdict, and no false claim")))

(deftest a-commented-out-step-is-not-a-step
  (is (= [] (judged "(defn f [^String m] (-> (Model/builder) (.modelName m) #_(.logging) (.build)))"))
      "#_(.logging) in a chain — clojure-mcp core.clj:225"))

(deftest what-the-second-blind-pair-taught
  (testing "a hint on a collection literal is a MetaExpr and types nothing"
    (is (= [:reflection] (judged "(defn f [x] (ProcessBuilder. ^java.util.List [\"sh\" x]))"))
        "darling-toolkit bridge.clj:265 — the compiler reflected through the hint"))
  (testing "division of two longs is a Number, so what multiplies it boxes"
    (is (= [[:boxed-math 20]] (at "(defn f [a b] (int (* 100.0 (/ (count a) (count b)))))")))
    (is (= [] (kinds "(defn f [a b] (int (* 100.0 (/ (double (count a)) (count b)))))")))))

(deftest hints-inside-destructuring-are-hints
  (is (= [] (kinds "(defn f [] (let [{:keys [^java.io.Writer out]} @state] (.flush out)))")))
  (is (= [] (kinds "(defn f [{:keys [^String s]}] (.length s))")))
  (is (= [:reflection] (kinds "(defn f [{:keys [s]}] (.length s))"))))

(deftest what-kora-core-taught-blind
  ;; first score, before reading: boxed P 0.82 R 0.99, reflection P 0.71 R 0.88
  (testing "each arity of a multi-arity fn carries its own hints"
    (is (= [] (kinds "(defn f ([t] (f t 1)) ([^double t ^double p] (* (- 1 t) p)))")))
    (is (= [:boxed-math :boxed-math] (kinds "(defn f ([t] (f t 1)) ([t p] (* (- 1 t) p)))"))))
  (testing "the compiler never narrows on a predicate — occurrence typing retracted"
    (is (= [:reflection] (kinds "(defn f [s] (when (and (string? s) (.startsWith s \"$\")) s))")))
    (is (= [:reflection] (kinds "(defn f [x] (if (instance? String x) (.length x) 0))"))))
  (testing "max and min are nary: three arguments warn"
    (is (= [:boxed-math] (kinds "(defn f [r g b] (max r g b))"))))
  (testing "a numeric static with a boxed Number reflects like any other — Math/abs on kora, 22 of 22"
    (is (= #{[:boxed-math 28] [:reflection 18] [:boxed-math 13]}
           (set (at* "(defn f [x] (- 1 (Math/abs (* 2 x))))" {:classes {:classes {"java.lang.Math" {:supers ["Object"] :methods {"abs" [{:params ["int"] :returns "int" :static? true} {:params ["long"] :returns "long" :static? true} {:params ["double"] :returns "double" :static? true}]}}} :by-simple {"Math" ["java.lang.Math"]}}})))
        "the multiply boxes, abs reflects on its Number, the subtract boxes over abs's Object")))

(deftest a-double-operand-makes-the-result-double-even-beside-an-unknown
  ;; Numbers.divide(Object, double) coerces and returns double: the divide is
  ;; warned, and everything over its result is primitive — kora color.clj
  (is (= [[:boxed-math 21]] (at "(defn f [l] (let [n (/ l 100.0)] (- 1 (* 2 n))))"))
      "only the divide boxes; n is a double, (* 2 n) and (- 1 …) are primitive"))

(deftest every-top-level-form-is-compiled
  (is (= [:boxed-math] (kinds "(register-converter :k (fn [bpm] (/ 60000 bpm)))"))
      "kora.core translation.clj — ten misses inside one register-converter")
  (is (= [] (kinds "(comment (defn f [a b] (+ a b)))")) "a comment form is not compiled"))

(deftest an-exact-overload-wins-over-a-widening-one
  (is (= [] (at* "(defn f [^double d] (- 1 (Math/abs d)))" {:classes {:classes {"java.lang.Math" {:supers ["Object"] :methods {"abs" [{:params ["float"] :returns "float" :static? true} {:params ["double"] :returns "double" :static? true}]}}} :by-simple {"Math" ["java.lang.Math"]}}}))
      "a double fits abs(float) too; the compiler takes abs(double) exactly, and the answer is a primitive double"))

(deftest the-js-host-warns-on-an-untyped-target-with-a-non-extern-property
  ;; cljs.analyzer/analyze-dot + shadow-cljs :infer-externs :auto; viewer at 92270f9^, 42/50
  (let [js (fn [src] (mapv :kind (remove #(= :reflection-unwarned (:kind %)) (tf/predictions src "x.cljs" :js {:externs #{"beginPath" "length"}}))))]
    (is (= [:uninferred] (js "(defn f [d] (.-sameNs d))")))
    (is (= [] (js "(defn f [ctx] (.beginPath ctx))")) "an extern property is silent whatever the target")
    (is (= [] (js "(defn f [^js d] (.-sameNs d))")))
    (is (= [] (js "(defn f [] (.-sameNs js/window))")))
    (is (= [] (js "(ns v (:require [\"d3\" :as d3]))\n(defn f [] (.-interpolateBlues d3))")) "a string-required alias is js")
    (is (= [] (js "(ns v (:require [\"d3\" :as d3]))\n(defn f [] (.interpolator (d3/scaleSequential)))")) "a call through the alias is a js value")))

(deftest inferred-return-tags-are-the-producer-for-the-inferred-rung
  (let [vt {"clojure.core/str" "java.lang.String"}]
    (is (= [{:name "f" :line 2 :slot -1 :type "String"}]
           (tf/inferred "(ns m)\n(defn f [x] (str x))" "m.clj" {:var-tags vt})))
    (is (= [{:name "g" :line 2 :slot -1 :type "long"}] (tf/inferred "(ns m)\n(defn g [^long n] (inc n))" "m.clj")))
    (is (= [] (tf/inferred "(ns m)\n(defn ^String h [x] (str x))" "m.clj" {:var-tags vt})) "a hinted return is declared, not inferred")
    (is (= [] (tf/inferred "(ns m)\n(defn k [x] (first x))" "m.clj")) "Object is not a fact")
    (is (= [{:name "m" :line 2 :slot -1 :type "String"} {:name "m" :line 2 :slot -1 :type "long"}]
           (tf/inferred "(ns m)\n(defn m ([x] (str x)) ([^long a ^long b] (+ a b)))" "m.clj" {:var-tags vt}))
        "arities that disagree are both returned — a conflict is a fact")))

(deftest an-unqualified-head-is-this-namespace-s-var-before-core-s
  ;; sonar-clojure junit.clj: (.newDocumentBuilder (safe-factory)) on a
  ;; ^DocumentBuilderFactory defn- in the same file, without kondo
  (is (= [] (kinds* "(ns s)\n(defn f [] (.newDocumentBuilder (safe-factory)))" {:var-tags {"s/safe-factory" "javax.xml.parsers.DocumentBuilderFactory"}})))
  (is (= [:reflection] (kinds "(ns s)\n(defn f [] (.newDocumentBuilder (safe-factory)))"))))

