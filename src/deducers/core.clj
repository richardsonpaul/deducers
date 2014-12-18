(ns deducers.core)

(defprotocol Functor
  (fmap [this f]))

(extend-protocol Functor
  ;; TODO - Not adhering to Functor: List
  clojure.lang.Seqable
  (fmap [this f]
    (into (empty this) (map f) this))
  clojure.lang.IPersistentMap
  (fmap [this f]
    (into {} (map (fn [[k v]] [k (f v)])) this))
  clojure.lang.Fn
  (fmap [this f]
    (comp f this))
  java.lang.Object
  (fmap [this f]
    (f this)))

(defprotocol Applicative
  (<*> [af f]))

(defprotocol Monad
  (join [nested]))

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
  Applicative
  (<*> [_ o] (-> (fmap o just)))
  Monad
  (join [_] just))

(defmethod print-method Just [m w]
  (print-method (symbol (str m)) w))

(defrecord Writer [v out]
  Functor
  (fmap [_ fn]
    (Writer. (fn v) out))
  Applicative
  Monad
  (join [_]
    (Writer. (:v v) (clojure.string/join "; " [out (:out v)]))))

(extend-type nil
  Maybe
  (maybe [_])
  Functor
  (fmap [this f])
  Applicative
  (<*> [_ _])
  Monad
  (join [_]))

(extend-type clojure.lang.ISeq
  Functor
  (fmap [this f]
    (apply list (map f this))))
