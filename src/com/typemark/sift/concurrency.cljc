(ns com.typemark.sift.concurrency
  "The concurrency hazards Clojure actually has.

  Most of Java's concurrency rules do not port: there is no `synchronized`
  to forget, no `volatile` to miss, no double-checked locking to get wrong,
  and shared data is immutable by default. That is the language doing its
  job, and reproducing those rules here would be theatre.

  What survives is narrower and specific to Clojure's coordination
  primitives, and it is the kind of defect that passes every test on a quiet
  machine and only appears under contention."
  (:require [com.typemark.sift.tree :as tree]))

(def ^:private retrying #{"swap!" "swap-vals!" "alter" "commute" "alter-var-root"})

(def ^:private effectful
  "Calls whose whole point is the effect. Deliberately short: a false
  positive here accuses correct code of a race that is not there."
  #{"println" "print" "prn" "pr" "spit" "slurp" "log/info" "log/warn" "log/error"
    "log/debug" "send" "send-off" "deliver" "reset!" "swap!" "conj!" "assoc!"
    "dissoc!" "disj!" "persistent!" "future" "sh" "execute!" "insert!" "delete!"
    "http/post" "http/get" "printf" "flush"})

(defn- effectful-call-inside?
  "A `reset!` or `deliver` on an atom or promise the enclosing `let` made is
  idempotent under retry — the CAS-with-a-decision idiom, lume's
  idempotency.clj: (let [decision (atom nil)] (swap! state (fn [m] … (reset!
  decision [:fresh]) …))). A retry resets the same local to the same value."
  [nodes locals form]
  (some #(and (= :list (:tag %))
              (contains? effectful (:head %))
              (not (and (contains? #{"reset!" "swap!" "deliver" "vreset!" "vswap!"} (:head %))
                        (contains? @locals (:text (tree/first-argument nodes %))))))
        (tree/children-of nodes form)))

(defn findings
  "Concurrency findings for one parsed file."
  [nodes]
  (concat
   (let [locals (delay (tree/let-bound-locals nodes #{"atom" "promise" "volatile!"}))]
     (for [n (tree/lists-headed-by nodes retrying)
           :when (effectful-call-inside? nodes locals n)]
       {:rule "side-effect-in-swap"
        :line (:line n) :col (:col n) :end-line (:end-line n) :end-col (:end-col n)
        :message (str (:head n) " retries under contention, so the side effect inside it"
                      " can happen more than once")}))

   (for [n (tree/lists-headed-by nodes #{"future"})
         :let [parent (->> nodes
                           (filter #(and (= :list (:tag %))
                                         (< (:depth %) (:depth n))
                                         (<= (:line %) (:line n))
                                         (>= (:end-line %) (:end-line n))))
                           (sort-by :depth)
                           last)]
         :when (and parent (contains? #{"do" "when" "when-let" "doseq" "dotimes"} (:head parent)))]
     {:rule "discarded-future"
      :line (:line n) :col (:col n) :end-line (:end-line n) :end-col (:end-col n)
      :message "the value of this future is discarded, so an exception inside it is never seen"})))
