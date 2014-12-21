(ns deducers.core
  (:require clojure.edn))

;; Protocols

(defprotocol ApplySpecial
  (apply-to [this f]))

(extend-protocol ApplySpecial
  clojure.lang.Seqable
  (apply-to [this f]
    (into (empty this) (map f) this))
  clojure.lang.IPersistentMap
  (apply-to [this f]
    (into {} (map (fn [[k v]] [k (f v)])) this))
  clojure.lang.Fn
  (apply-to [this f]
    (comp f this)))

(defprotocol Deducer
  (handle-nested [nested]))

(defprotocol DeducingFn
  (deduce-with [this f]))

(defprotocol Accumulator
  (accum [this x]))

(defn >>=
  ([mv [mf & mfs]]
     (if mf
       (recur (deduce-with mv mf) mfs)
       mv))
  ([df mv [mf & mfs]]
     (df)))

(defn- process [[[v b] & more] body]
  `(>>= ~b
        (fn [~v]
          ~(if more
             (process more body)
             body))))

(defmacro deduce [defs & body]
  (process (partition 2 defs) (cons `do body)))

(defmacro deduce-using [wrap-fn defs & body]
  (list `deduce
         (interleave
          (take-nth 2 defs)
          (->> defs (drop 1) (take-nth 2) (map #(list wrap-fn %))))
         (list wrap-fn (list* `do body))))

(defn deducing-fn [f v]
  (-> mv (apply-to f) handle-nested))

;;;; Maybe: safe failure

(defprotocol Maybe
  (maybe? [this]))

(deftype Just [just]
  java.lang.Object
  (toString [_] (str "#<Just " just ">"))
  (hashCode [_] (hash just))
  (equals [_ other] (and (not (nil? other))
                            (instance? Just other)
                            (= just (.just other))))
  Maybe
  (maybe? [_] just)
  ApplySpecial
  (apply-to [_ f] (-> (f just) Just.))
  Deducer
  (handle-nested [_] just))

(defmethod print-method Just [m w]
  (print-method (symbol (str m)) w))

(defn maybe [x]
  (if x (Just. x)))

;; Writer

(defrecord Acc [acc value]
  ApplySpecial
  (apply-to [_ func] (Acc. acc (func value)))
  Deducer
  (handle-nested [_]
    (assoc value :acc (accum acc (:acc value)))))

;; Ad-hoc

(defrecord AdHocDeducer [a-to h-nest a-de value]
  ApplySpecial
  (apply-to [this f]
    (update this :value assoc (a-to value f)))
  Deducer
  (handle-nested [this]
    (update this :value assoc h-nest))
  DeducingFn
  (deduce-with [this f]))

;; Extensions on clojure.core

(extend-type nil
  Maybe
  (maybe? [_])
  ApplySpecial
  (apply-to [this f])
  Deducer
  (handle-nested [_]))

(extend Object
  Maybe
  {:maybe? identity}
  ApplySpecial
  {:apply-to (fn [o f] (Just. (f o)))}
  Deducer
  {:handle-nested identity}
  DeducingFn
  {:deduce-with deducing-fn})

(extend-type clojure.lang.ISeq
  ApplySpecial
  (apply-to [this f]
    (apply list (map f this)))
  Deducer
  (handle-nested [nested]
    (apply concat nested)))

(extend-type String
  ApplySpecial
  (apply-to [s f] (pr-str (f (clojure.edn/read-string s))))
  Deducer
  (handle-nested [s] (clojure.edn/read-string s))
  Accumulator
  (accum [this x] (str this x)))

;; Miscellanea

(defmacro let-safe [bindings & forms]
  `(maybe? (deduce-using maybe ~bindings ~@forms)))
