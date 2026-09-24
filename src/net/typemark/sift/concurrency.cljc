(ns net.typemark.sift.concurrency
  (:require [net.typemark.sift.tree :as tree]))

(def ^:private retrying #{"swap!" "swap-vals!" "alter" "commute" "alter-var-root"})

(def ^:private effectful
  #{"println" "print" "prn" "pr" "spit" "slurp" "log/info" "log/warn" "log/error"
    "log/debug" "send" "send-off" "deliver" "reset!" "swap!" "conj!" "assoc!"
    "dissoc!" "disj!" "persistent!" "future" "sh" "execute!" "insert!" "delete!"
    "http/post" "http/get" "printf" "flush"})

(defn- effectful-call-inside?
  [nodes locals form]
  (some #(and (= :list (:tag %))
              (contains? effectful (:head %))
              (not (and (contains? #{"reset!" "swap!" "deliver" "vreset!" "vswap!"} (:head %))
                        (contains? @locals (:text (tree/first-argument nodes %))))))
        (tree/children-of nodes form)))

(defn findings
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
