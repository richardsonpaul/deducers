(ns deducers.core
  (:require clojure.edn))

;; Protocols

(defprotocol ApplySpecial
  (apply-to [this f]))

(extend-protocol ApplySpecial
  clojure.lang.IPersistentMap
  (apply-to [this f]
    (into {} (map (fn [[k v]] [k (f v)])) this))
  clojure.lang.Fn
  (apply-to [this f]
    (comp f this)))

(defprotocol Deducer
  (handle-nested [nested]))

(defprotocol ^:private Binding
             "Dispatch >>= polymorphically"
             (bind
               [seq-or-obj v]
               [seq-or-obj v d]))

(defprotocol Accumulator
  (accum [this x]))

(defn >>=
  "Iteratively deduce the value mv throught the function(s) mfs,
  wrapping with the given deducer constructor d, if given.
  Can pass a Seqable of fns or a single fn for mfs, dispatches polymorphically."
  ([mv mfs]
   (bind mfs mv))
  ([mv d mfs]
   (if-not d
     (bind mfs mv)
     (bind mfs mv d))))

(defn- process-deduce [deducer bindings body]
  (letfn [(process [[[v b] & bindings]]
            `(>>= (~deducer ~b)
                  ~(fn-body v bindings)))
          (fn-body [v bindings]
            `(fn [~v]
               ~(if bindings
                  (process bindings)
                  `(~deducer (do ~@body)))))]
    (process bindings)))

(defmacro deduce [bindings & exprs]
  `(deduce-with nil ~bindings ~@exprs))

(defmacro deduce-with [deducer bindings & exprs]
  (process-deduce deducer (partition 2 bindings) exprs))

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
    (update this :value assoc h-nest)))

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
;  ApplySpecial
;  {:apply-to (fn [o f] (Just. (f o)))}
  Binding
  {:bind (fn
           ([mf mv d]
            (cond-> (bind mf mv) d d))
           ([mf mv]
            (-> mv (apply-to mf) handle-nested)))})

(extend-type clojure.lang.Seqable
  ApplySpecial
  (apply-to [this f]
    (into (empty this) (map f) this))
  Binding
  (bind
    ([[mf & mfs] v d]
     (if mf
       (recur mfs (bind mf v d) d)
       v))
    ([mfs v]
     (bind mfs v nil))))

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
  `(maybe? (deduce-with maybe ~bindings ~@forms)))
