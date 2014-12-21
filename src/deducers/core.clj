(ns deducers.core
  (:require clojure.edn))

;; Protocols

(defprotocol Functor
  (fmap [this f]))

(extend-protocol Functor
  clojure.lang.IPersistentMap
  (fmap [this f]
    (into {} (map (fn [[k v]] [k (f v)])) this))
  clojure.lang.Fn
  (fmap [this f]
    (comp f this)))

(defprotocol Monad
  (join [nested]))

(defprotocol ^:private Binding
             "Dispatch >>= polymorphically"
             (bind
               [seq-or-obj v]
               [seq-or-obj v d]))

(defprotocol Accumulator
  (accumulate [this x]))

(defn >>=
  "Iteratively deduce the value mv throught the function(s) mfs,
  wrapping with the given deducer constructor d, if given.
  Can pass a Seqable of fns or a single fn for mfs, dispatches polymorphically."
  ([mv mfs]
   (bind mfs mv))
  ([mv d mfs]
   (bind mfs mv d)))

(defn- process [deducer [[v b] & more] body]
  `(>>= ~b
        (fn [~v]
          ~(if more
             (process deducer more body)
             (cons `do body)))
        deducer))

(defmacro deduce [bindings & exprs]
  `(deduce-with nil ~bindings ~@exprs))

(defmacro deduce-with [deducer bindings & exprs]
  (process deducer (partition 2 bindings) exprs))

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
  Functor
  (fmap [_ f] (-> (f just) Just.))
  Monad
  (join [_] just))

(defmethod print-method Just [m w]
  (print-method (symbol (str m)) w))

(defn maybe [x]
  (if x (Just. x)))

;; Writer

(defrecord Acc [acc value]
  Functor
  (fmap [_ func] (Acc. acc (func value)))
  Monad
  (join [_]
    (assoc value :acc (accumulate acc (:acc value)))))

;; Extensions on clojure.core

(extend-type nil
  Maybe
  (maybe? [_])
  Functor
  (fmap [this f])
  Monad
  (join [_]))

(extend Object
  Maybe
  {:maybe? identity}
  Functor
  {:fmap (fn [o f] (Just. (f o)))}
  Monad
  {:join identity}
  Binding
  {:bind (fn
           ([mf mv d]
            (let [result (bind mf mv)]
              (if d
                (d result)
                result)))
           ([mf mv]
            (foo mv mf)))})

(extend-type clojure.lang.Seqable
  Functor
  (fmap [this f]
    (into (empty this) (map f) this))
  Binding
  (bind [[mf & mfs] v d]
    (recur mfs (bind mf v d)))
  (bind [mfs v]
    (bind mfs v nil)))

(extend-type clojure.lang.ISeq
  Functor
  (fmap [this f]
    (apply list (map f this)))
  Monad
  (join [nested]
    (apply concat nested)))

(extend-type String
  Functor
  (fmap [s f] (pr-str (f (clojure.edn/read-string s))))
  Monad
  (join [s] (clojure.edn/read-string s))
  Accumulator
  (accumulate [this x] (str this x)))

;; Miscellanea

(defmacro let-safe [bindings & forms]
  `(maybe? (deduce-with maybe ~bindings ~@forms)))
