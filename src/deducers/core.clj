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
    (comp this f))
  java.lang.Object
  (fmap [this f]
    (f this)))

(defn functor?
  "Test an object to see if it adheres to the functor laws.
  f and g are functions that operate on the functor"
  [ftor f g]
  (let [id (fmap ftor identity)
        composed (fmap ftor (comp f g))
        nested (fmap (fmap ftor g) f)
        follows-laws? (and (= id (identity ftor)) (= composed nested))]
    (when-not follows-laws?
      (println "Identity fmapped:" id)
      (println "Composed:" (str composed "; Nested:") nested))
    follows-laws?))

(defprotocol Applicative
  (<*> [af f]))

(def ^:dynamic *pure-fn* identity)

(defprotocol Monad
  (bind [this f])
  (join [this]))

(defn >>= [mv & [mf & mfs]]
  (if mf
    (recur (bind mv mf) mfs)
    mv))

(defn- process [[[v b] & more] body]
  `(bind ~b (fn [~v] ~(if more (process more body) body))))

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
  (bind [_ f] (binding [*pure-fn* #(Just. %)] (-> just f Just. join)))
  (join [_] just))

(defmethod print-method Just [m w]
  (print-method (symbol (str m)) w))

(extend-type nil
  Maybe
  (maybe [_])
  Functor
  (fmap [this f])
  Applicative
  (<*> [_ _])
  Monad
  (bind [_ _]))
