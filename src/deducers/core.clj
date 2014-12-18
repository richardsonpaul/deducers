(ns deducers.core
  (:require clojure.edn))

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
  `(>>= ~b (fn [~v] ~(if more (process more body) body))))

(defmacro deduce [defs expr]
  (process (partition 2 defs) expr))

;;;; Maybe: safe failure

(defprotocol Maybe
  (maybe [this]))

(deftype Just [just]
  java.lang.Object
  (toString [_] (str "#<Just " just ">"))
  (hashCode [_] (hash just))
  (equals [_ other] (and (not (nil? other))
                            (instance? Just other)
                            (= just (.just other))))
  Maybe
  (maybe [_] just)
  Functor
  (fmap [_ f] (-> (f just) Just.))
  Monad
  (join [_] just))

(defmethod print-method Just [m w]
  (print-method (symbol (str m)) w))

;; Writer

(defrecord Acc [v a]
  Functor
  (fmap [_ f] (Acc. (f v) a))
  Monad
  (join [nested]
    (Acc. (:v v) (accumulate a (:a v)))))

;; Extensions on clojure.core

(extend-type nil
  Maybe
  (maybe [_])
  Functor
  (fmap [this f])
  Monad
  (join [_]))

(extend Object
  Maybe
  {:maybe identity}
  Functor
  {:fmap (fn [o f] (f o))}
  Monad
  {:join identity})

(extend-type clojure.lang.ISeq
  Functor
  (fmap [this f]
    (apply list (map f this))))

(extend-type String
  Functor
  (fmap [s f] (pr-str (f (clojure.edn/read-string s))))
  Monad
  (join [s] (clojure.edn/read-string s))
  Accumulator
  (accumulate [this x] (str this x)))
