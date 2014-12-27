(ns deducers.core
  (:require clojure.edn))

;; Protocols

(defprotocol ApplySpecial
  "Specifies how to apply a deducer to a 'regular' fn."
  (apply-to [this f]))

(extend-protocol ApplySpecial
  clojure.lang.IPersistentMap
  (apply-to [this f]
    (into {} (map (fn [[k v]] [k (f v)])) this))
  clojure.lang.Fn
  (apply-to [this f]
    (comp f this)))

(defprotocol FlexDeducer
  "A FlexDeducer needs to also provide ApplySpecial/apply-to
   as part of its implementation. Applying a Deducer to a deducible
  function will end up returning a deducer with some kind of nesting.
  This function un-nests and folds in the return value with the nested one."
  (handle-nested [nested]))

(defprotocol SimpleDeducer
  "A deducer contained in one function that both applies the Deducer
   to the fn arg, and fixes up the return value, all in one go."
  (deduce [v f]))

(defprotocol Accumulator
  "An accumulator tells how to fold two values together. This can be viewed
   like a fn passable to reduce, which takes two values and accumulates one
  into the other. \"accum\" takes two values of the same type and returns
  the same type, with the two args combined/folded together as the result."
  (accum [this x]))

(defprotocol ^:private Binding
             "Dispatch >>= polymorphically"
             (bind
               [seq-or-obj v]
               [seq-or-obj v d]))

(extend-protocol Binding
  Object
  (bind
    ([mf mv d]
     (-> (cond->> mf d (comp d))
         (bind (cond-> mv d d))))
    ([mf mv]
     (-> mv (apply-to mf) handle-nested)))
  clojure.lang.Seqable
  (bind
    ([[mf & mfs] v d]
     (if mf
       (recur mfs (bind mf v d) d)
       v))
    ([mfs v]
     (bind mfs v nil))))

(defn >>=
  "Iteratively deduce the value mv throught the function(s) mfs,
  wrapping with the given deducer constructor d, if given.
  Can pass a Seqable of fns or a single fn for mfs, dispatches polymorphically."
  ([mv mfs]
   (bind mfs mv))
  ([mv d mfs]
   (bind mfs mv d)))

(defn- process [[[v b] & more] body]
  `(>>= ~b
        (fn [~v]
          ~(if more
             (process more body)
             body))))

(defmacro let-deduce [defs & body]
  (process (partition 2 defs) (cons `do body)))

(defmacro deduce-with [wrap-fn defs & body]
  (list `let-deduce
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
  ApplySpecial
  (apply-to [_ f]
    (-> (f just) Just.))
  FlexDeducer
  (handle-nested [_] just))

(defmethod print-method Just [m w]
  (print-method (symbol (str m)) w))

(defn maybe [x]
  (if x (Just. x)))

;; Writer

(defrecord Acc [acc value]
  ApplySpecial
  (apply-to [_ func] (Acc. acc (func value)))
  FlexDeducer
  (handle-nested [_]
    (assoc value :acc (accum acc (:acc value)))))

;; Ad-hoc

(defrecord AdHoc [apply-to handle-nested value]
  ApplySpecial
  (apply-to [this f]
    (assoc this :value (apply-to value f)))
  FlexDeducer
  (handle-nested [this]
    (-> this
        (update-in [:value 1] :value)
        (update :value handle-nested)
        :value)))

(defn deducer
  "Constructs an ad-hoc deducer specified by the args:
   A single fn as arg acts as the deduce function in a SimpleDeducer.
   Multiple args specify a FlexDeducer, and
    should be given as keyword -> fn where the supported
    keywords are :apply-special and :handle-nested"
  [& {:keys [apply-special handle-nested] :as args}]
  #(map->AdHoc (merge args) {:value %}))
;; Extensions on clojure.core

(extend-type nil
  Maybe
  (maybe? [_])
  ApplySpecial
  (apply-to [this f])
  FlexDeducer
  (handle-nested [_]))

(extend Object
  Maybe
  {:maybe? identity}
  ApplySpecial
  {:apply-to (fn [o f] (Just. (f o)))})

;; Vector and Queue, but not List, Conses, lazy-seqs, or other front-appending seqs
(extend-type clojure.lang.Seqable
  ApplySpecial
  (apply-to [this f]
    (into (empty this) (map f) this))
  FlexDeducer
  (handle-nested [this]
    (into (empty this) (apply concat this))))

(extend-type clojure.lang.ISeq
  ApplySpecial
  (apply-to [this f]
    (apply list (map f this)))
  FlexDeducer
  (handle-nested [nested]
    (apply concat nested)))

(extend-type String
  ApplySpecial
  (apply-to [s f] (pr-str (f (clojure.edn/read-string s))))
  FlexDeducer
  (handle-nested [s] (clojure.edn/read-string s))
  Accumulator
  (accum [this x] (str this x)))

;; Miscellanea

(defmacro let-safe [bindings & forms]
  `(maybe? (deduce-with maybe ~bindings ~@forms)))
