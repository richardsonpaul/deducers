(ns deducers.core
  (:require clojure.edn))

;; Protocols

(defprotocol Functor
  (fmap [this f]))

(extend-protocol Functor
  clojure.lang.Seqable
  (fmap [this f]
    (into (empty this) (map f) this))
  clojure.lang.IPersistentMap
  (fmap [this f]
    (into {} (map (fn [[k v]] [k (f v)])) this))
  clojure.lang.Fn
  (fmap [this f]
    (comp f this)))

(defprotocol Monad
  (join [nested]))

(defprotocol Accumulator
  (accumulate [this x]))

(defn >>= [mv & [mf & mfs]]
  (if mf
    (recur (-> mv (fmap mf) join) mfs)
    mv))

(defn- process [[[v b] & more] body]
  `(>>= ~b
        (fn [~v]
          ~(if more
             (process more body)
             body))))

(defmacro deduce [defs & body]
  (process (partition 2 defs) (cons `do body)))

(defmacro deduce-with [wrap-fn defs & body]
  (list `deduce
         (interleave
          (take-nth 2 defs)
          (->> defs (drop 1) (take-nth 2) (map #(list wrap-fn %))))
         (list wrap-fn (list* `do body))))

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
  {:join identity})

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
