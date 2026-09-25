(ns net.typemark.sift.registry
  (:require [net.typemark.sift.callgraph :as callgraph]
            [net.typemark.sift.comments :as comments]
            [net.typemark.sift.concurrency :as concurrency]
            [net.typemark.sift.cond-build :as cond-build]
            [net.typemark.sift.cond-case :as cond-case]
            [net.typemark.sift.fold :as fold]
            [net.typemark.sift.host :as host]
            [net.typemark.sift.interop :as interop]
            [net.typemark.sift.let-chain :as let-chain]
            [net.typemark.sift.loop-fold :as loop-fold]
            [net.typemark.sift.map-loop :as map-loop]
            [net.typemark.sift.portable.prose :as prose]
            [net.typemark.sift.regex :as regex]
            [net.typemark.sift.security :as security]
            [net.typemark.sift.tenancy :as tenancy]
            [net.typemark.sift.tests :as tests]
            [net.typemark.sift.typeflow :as typeflow]
            [net.typemark.sift.web :as web]))

(def checks #{:existence :substitution :metric :script})

(def levels [:off :info :warning :error])

(def rungs #{:compiler :parity :corpus :read :unjudged})

(def applicability #{:machine-applicable :maybe-incorrect :has-placeholders :unspecified})

(def rulesets
  {:correctness :error
   :suspicious  :warning
   :security    :warning
   :performance :info
   :complexity  :warning
   :style       :warning
   :tests       :warning
   :doc         :warning
   :tenancy     :warning})

(def default-rulesets
  #{:correctness :suspicious :security :performance :complexity :style :tests :doc})

(defn- on-nodes [f] (fn [file _] (f (:nodes file))))

(defn- on-zloc [f] (fn [file _] (f (:zloc file))))

(defn- judged
  [file hit]
  (cond
    (not= :jvm (:host file)) hit
    (nil? (:classes file)) (assoc hit :evidence :unjudged)
    (and (:loaded file) (not ((:loaded? file) (:path file)))) (assoc hit :evidence :unjudged)
    :else hit))

(defn- typeflow-rule
  [kind compiled?]
  (fn [file _]
    (for [p (:predictions @(:typeflow file)) :when (= kind (:kind p))]
      (cond->> (typeflow/hit p) compiled? (judged file)))))

(defn- doc-rule
  [id]
  (fn [file _] (filter #(= id (:rule %)) @(:prose file))))

(defn- taint
  [linter files _]
  (let [seeds (reduce (fn [acc f] (merge-with into acc (security/seeds (:nodes f))))
                      {:taints #{} :reaches #{}}
                      (filter :nodes files))]
    (when-not (or (empty? (:taints seeds)) (empty? (:reaches seeds)))
      (callgraph/findings (:kondo linter) seeds))))

(def built-in
  [{:id :correctness/deref-inside-own-swap
    :extends :existence :evidence :corpus
    :either '[(clojure.core/deref ?a) (deref ?a)]
    :inside '(swap! ?a ?&_)
    :message "the atom is read while its own swap! is computed — a lost update"
    :instruction "Inside (swap! a f), f receives the current value — use it. In the arguments, move the read into f: (swap! a update k merge …). A second @a may be older, and f may run more than once."}
   {:id :correctness/js-prop-on-own-object
    :extends :script :evidence :corpus
    :run (typeflow-rule :js-prop-on-own-object false)
    :instruction "Read your own #js object with (aget obj \"k\"): #js writes a quoted key and .-k a renamable one, and :advanced renames one side."}
   {:id :correctness/uninferred
    :extends :script :evidence :compiler :needs #{:oracle}
    :run (typeflow-rule :uninferred true)
    :instruction "Hint the receiver ^js, or reach the global through js/; Closure renames what it cannot infer."}

   {:id :correctness/blocking-inside-go
    :extends :existence :evidence :corpus :scope :any
    :either '[(clojure.core.async/<!! ?c) (clojure.core.async/>!! ?c ?v)
              (clojure.core.async/alts!! ?&_) (clojure.core.async/alt!! ?&_)
              (<!! ?c) (>!! ?c ?v) (alts!! ?&_) (alt!! ?&_)]
    :inside '[(clojure.core.async/go ?&_) (clojure.core.async/go-loop ?&_) (go ?&_) (go-loop ?&_)]
    :message "a blocking take or put inside a go block parks a thread of a finite pool"
    :instruction "Use the parking form — <! >! alts! — inside go. Blocking belongs outside it, or inside (a/thread …), which is an unbounded pool."}
   {:id :correctness/case-else
    :extends :existence :evidence :corpus :scope :body
    :match '(case ?x (?? ?k ?e) ... :else ?d)
    :message ":else is a TEST VALUE in case, not a default — it fires only for the input :else"
    :instruction "case has no :else. Put the default in the trailing position with no test beside it, or use cond."}
   {:id :correctness/ns-load-side-effect
    :extends :existence :evidence :corpus :scope :top
    :either '[(require ?&_) (use ?&_) (requiring-resolve ?&_)]
    :message "a require outside the ns form is a dependency the build graph does not have"
    :instruction "Declare it in the ns form. A requiring-resolve INSIDE a function is the idiom for breaking a cycle and is not this."}
   {:id :correctness/rt-internal
    :extends :existence :evidence :read :scope :any
    :head-ns "clojure.lang.RT"
    :message "clojure.lang.RT is Clojure's internals, not its API"
    :instruction "Use the public var. RT has no compatibility promise and a release can move a method without notice."}

   {:id :suspicious/side-effect-in-swap
    :extends :script :evidence :read :run (on-nodes concurrency/side-effect-in-swap)}
   {:id :suspicious/discarded-future
    :extends :script :evidence :corpus :run (on-nodes concurrency/discarded-future)}
   {:id :suspicious/catch-all-swallow
    :extends :script :evidence :corpus :run (on-zloc host/catch-all-swallow)
    :instruction "Do not turn a host exception into a constant. Return the failure as data — (ex-info …), {:error …}, or nil with the cause logged — or let it propagate."}
   {:id :suspicious/mutable-escape
    :extends :script :evidence :corpus :run (on-zloc host/mutable-escape)
    :instruction "Do not return a host mutable. Convert at the boundary — (vec …), (into {} …), (js->clj …) — so callers receive a value."}

   {:id :suspicious/nested-reference
    :extends :existence :evidence :corpus :scope :any
    :either '[(clojure.core/atom ?&_) (atom ?&_) (ref ?&_) (agent ?&_) (volatile! ?&_)]
    :inside '[(clojure.core/atom ?&_) (atom ?&_) (ref ?&_)]
    :message "a reference type inside another — no snapshot of the whole is consistent"
    :instruction "Hold one value in one identity. Updating the inner reference does not change the outer one, so no reader ever sees a coherent whole."}

   {:id :performance/boxed-math
    :extends :script :evidence :compiler :needs #{:oracle}
    :run (typeflow-rule :boxed-math true)
    :instruction "Hint the receiver or operands (^String s, ^long n), or cast (long x); the host compiler takes the slow path where the tag runs out."}
   {:id :performance/reflection
    :extends :script :evidence :compiler :needs #{:oracle}
    :run (typeflow-rule :reflection true)
    :instruction "Hint the receiver or operands (^String s, ^long n), or cast (long x); the host compiler takes the slow path where the tag runs out."}
   {:id :performance/thread-sleep
    :extends :existence :evidence :corpus
    :either '[(Thread/sleep ?&args) (java.lang.Thread/sleep ?&args)]
    :message "Thread/sleep blocks a host thread"
    :instruction "Schedule with a timer, a core.async timeout, or an executor; a blocked thread is a held resource."}

   {:id :security/shell-invocation
    :extends :script :evidence :corpus :run (on-nodes security/shell-invocation)}
   {:id :security/process-spawn
    :extends :existence :evidence :corpus
    :match '(java.lang.ProcessBuilder. ?&args)
    :message "process spawned — confirm no argument is caller-controlled"
    :instruction "Pass the command as a literal vector and the untrusted part as an argument, never through a shell string."}
   {:id :security/hardcoded-credential
    :extends :script :evidence :read :run (on-nodes security/hardcoded-credential)}
   {:id :security/xml-external-entity
    :extends :script :evidence :corpus
    :run (fn [file _] (concat (security/xml-parse (:nodes file)) (interop/xml-external-entity (:nodes file))))}
   {:id :security/permissive-file-permissions
    :extends :script :evidence :read :run (on-nodes security/permissive-file-permissions)}
   {:id :security/trust-all-certificates
    :extends :script :evidence :corpus :run (on-nodes interop/trust-all-certificates)}
   {:id :security/redos-vulnerable-regex
    :extends :script :evidence :corpus :run (on-nodes regex/redos-vulnerable-regex)}
   {:id :security/partial-match-validation
    :extends :script :evidence :corpus :run (on-nodes regex/partial-match-validation)}
   {:id :security/xss-unescaped-output
    :extends :script :evidence :corpus :run (on-nodes web/xss-unescaped-output)}
   {:id :security/csrf-protection-absent
    :extends :script :evidence :read :run (on-nodes web/csrf-protection-absent)}
   {:id :security/sensitive-data-logged
    :extends :script :evidence :corpus :run (on-nodes web/sensitive-data-logged)}
   {:id :security/cookie-missing-security-flags
    :extends :script :evidence :corpus :run (on-nodes web/cookie-missing-security-flags)}
   {:id :security/jndi-injection
    :extends :existence :evidence :corpus
    :either '[(javax.naming.InitialContext/doLookup ?n) (.lookup ?ctx ?n) (.doLookup ?ctx ?n)]
    :when '{?n :dynamic ?ctx {:tag "javax.naming.Context"}}
    :message "JNDI lookup with a computed name"
    :instruction "A caller-controlled JNDI name is remote code execution on older JDKs and information disclosure on new ones; look up a literal name, or validate against an allow-list first."}
   {:id :security/unsafe-deserialization
    :extends :existence :evidence :corpus
    :either '[(java.io.ObjectInputStream. ?&args) (java.beans.XMLDecoder. ?&args)]
    :message "Java native deserialization"
    :instruction "ObjectInputStream and XMLDecoder run the classes in the stream; use a data format, or an ObjectInputFilter with an allow-list."}
   {:id :security/predictable-temp-file
    :extends :existence :evidence :corpus
    :match '(java.io.File/createTempFile ?&args)
    :message "File/createTempFile is predictable and world-readable by default"
    :instruction "Use java.nio.file.Files/createTempFile, which sets owner-only permissions."}
   {:id :security/interprocedural-taint
    :extends :script :evidence :unjudged :corpus true :needs #{:kondo} :run taint}

   {:id :complexity/place-as-fold
    :extends :script :evidence :corpus
    :run (fn [file _] (binding [fold/*resolve* (:resolution file)] (fold/findings (:zloc file))))
    :instruction "Do not accumulate in an atom. Use reduce (or into / group-by). Every branch, including else and catch, must return the accumulator. Do not swap! or reset!."}
   {:id :complexity/loop-as-map
    :extends :script :evidence :corpus :run (on-zloc map-loop/findings)
    :instruction "Do not walk a seq with loop/recur and conj onto an out vector. Use (into [] (map f) xs) or (into [] (comp (filter p) (map f)) xs). Keep the element transform; drop the out binding."}
   {:id :complexity/loop-as-reduce
    :extends :script :evidence :corpus
    :run (on-zloc loop-fold/findings)
    :instruction "Do not walk a seq with loop/recur to thread an accumulator. Use (reduce (fn [acc x] …) init coll). Keep the step expression; drop the seq binding and the exhaustion test."}
   {:id :complexity/first-filter-is-some
    :extends :substitution :evidence :corpus
    :match '(first (filter ?p ?xs))
    :emit '(some (fn [x] (when (?p x) x)) ?xs)
    :applicability :maybe-incorrect
    :message "(first (filter p xs)) walks no further than some does, and some says so"
    :instruction "Use (some (fn [x] (when (p x) x)) xs); a nil or false element is the one case first finds and some does not."}
   {:id :complexity/cognitive-complexity
    :extends :metric :evidence :parity :measure :cognitive :max 15
    :instruction "Split the unit: one branch per helper, or lift the nested lambda that carries the score."}

   {:id :style/cond-as-case
    :extends :script :evidence :corpus :run (on-zloc cond-case/findings)
    :instruction "Do not compare one value against literals clause by clause. Use case: (case x :a … :b … default). Keep the clause bodies; drop the (= x …) tests."}

   {:id :style/cond-as-build-up
    :extends :script :evidence :corpus :min 3
    :run (fn [file {:keys [min]}] (cond-build/findings (:zloc file) min))
    :instruction "Do not rebuild one name through a chain of conditional rebindings. Use cond->: (cond-> init test (f args) test (g args)). Each pair is a test and the step it guards."}
   {:id :style/let-as-thread
    :extends :script :evidence :corpus :min 3
    :run (fn [file {:keys [min]}] (let-chain/findings (:zloc file) min))
    :instruction "Do not name every intermediate value of one pipeline. Thread it: ->> when each step takes the value LAST, -> when it takes it first. The names are the shape, not information."}
   {:id :style/marker-protocol
    :extends :existence :evidence :corpus :scope :top
    :either '[(defprotocol ?n) (defprotocol ?n ?doc)]
    :when '{?doc :string?}
    :message "a defprotocol with no methods is a type tag wearing protocol machinery"
    :instruction "If it names a kind rather than a contract, a metadata key or a keyword in the value says so at no cost."}
   {:id :style/private-multimethod
    :extends :existence :evidence :corpus :scope :top
    :match '(defmulti ?n ?&_)
    :when '{?n :private-meta}
    :message "a private multimethod closes the one abstraction built to be open"
    :instruction "A multimethod exists so another namespace can extend it. If nothing may, a case or a map of functions says that plainly."}
   {:id :style/monolithic-ns-split
    :extends :existence :evidence :corpus :scope :top
    :either '[(load ?&_) (in-ns ?&_)]
    :message "one namespace split across files by load/in-ns is invisible to every build tool"
    :instruction "Make each file its own namespace and require it. load and in-ns bypass the dependency graph tools read."}

   {:id :tests/empty-test
    :extends :script :evidence :corpus :files :test :run (on-nodes tests/empty-test)}
   {:id :tests/testing-without-assertion
    :extends :script :evidence :corpus :files :test :run (on-nodes tests/testing-without-assertion)}
   {:id :tests/test-with-no-effect
    :extends :script :evidence :corpus :files :test :run (on-nodes tests/test-with-no-effect)}

   {:id :doc/restates-name
    :extends :script :evidence :corpus :run (doc-rule :doc/restates-name)
    :instruction "Say what the function guarantees or returns, not its name again: the input's shape, the edge case, what nil means."}
   {:id :doc/hedge
    :extends :script :evidence :corpus :run (doc-rule :doc/hedge)
    :instruction "Delete the hedge. A docstring is the contract; 'this function is used to' says nothing the name did not."}
   {:id :doc/params-unnamed
    :extends :script :evidence :corpus :run (doc-rule :doc/params-unnamed)
    :instruction "Name each parameter and what it must be; a reader at the call site has the arglist, not the body."}
   {:id :doc/placeholder
    :extends :script :evidence :corpus :run (doc-rule :doc/placeholder)
    :instruction "Write the docstring or remove the placeholder; a TODO docstring reads as documented in every tool."}
   {:id :doc/ns-missing
    :extends :script :evidence :corpus :level :off :run (doc-rule :doc/ns-missing)
    :instruction "A namespace docstring says what lives here and why it is separate; one sentence is enough."}
   {:id :doc/narrates-body
    :extends :script :evidence :read :level :info :min-words 3 :min-overlap 0.75
    :run (fn [file {:keys [min-words min-overlap]}] (prose/narrates-body @(:docs file) min-words min-overlap))
    :instruction "Say what the code cannot: the contract, the edge case, what nil means. A docstring made of the body's own names repeats it."}
   {:id :doc/docstring
    :extends :script :evidence :corpus :level :off
    :run (fn [file _] (for [d @(:docs file) :when (string? (:text d))]
                        (assoc (select-keys d [:line :column :end-line :end-column])
                               :symbol (some-> (:name d) symbol) :kind (:kind d) :message "docstring")))}
   {:id :doc/comment
    :extends :script :evidence :corpus :level :off
    :run (fn [file rule] (comments/findings (:nodes file) rule))}

   {:id :tenancy/ambiguous-owner-check
    :extends :script :evidence :read :required [:tenant-pattern]
    :run (fn [file {:keys [tenant-pattern]}]
           (tenancy/ambiguous-owner-check (:nodes file) (re-pattern tenant-pattern)))}
   {:id :tenancy/unscoped-tenant-query
    :extends :script :evidence :read :required [:tenant-pattern]
    :run (fn [file {:keys [tenant-pattern shared-namespaces]}]
           (tenancy/unscoped-tenant-query (:nodes file) (re-pattern tenant-pattern) (set shared-namespaces)))}])
