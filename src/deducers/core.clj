(ns deducers.core
  (:require clojure.edn))

;; Protocols

(defprotocol ApplySpecial
  "Specifies how to apply a deducer to a 'regular' fn."
  (apply-to [this f] "Apply a function f to this deducer"))

(extend-protocol ApplySpecial
  clojure.lang.Fn
  (apply-to [this f]
    (comp f this)))

(defprotocol FlexDeducer
  "A FlexDeducer needs to also provide ApplySpecial/apply-to as part
  of its implementation. Applying (apply-to) a Deducer to a deducing
  function will end up returning a deducer with some kind of nesting.
  This function un-nests and folds in the return value with the nested one."
  (handle-nested [nested] "Removes a level of nesting in the deducer"))

(defprotocol SimpleDeducer
  "A deducer contained in one function that both applies the Deducer
  to the fn arg, and fixes up the return value, all in one go. This ends
  up being the same thing, in effect, as (comp handle-nested apply-to)"
  (deduce [v f] "Applies the deducing function f to the deduced value v."))

(defprotocol MultiDeducer
  "A deducer that can apply multiple args to a function, and combine the results.
  d is a deducer arg, whose type will be dispatched on, and f is the fn to call.
  More is a seq of deducers (of the same type as d), and the fn will be called with
  all of them. This fn does the call, and combines the results. See accum"
  (apply-all [d f more]))

(defprotocol ValueDeducer
  "A deducer-value nested inside an unwrappable deducer, used in deducer->>="
  (unwrap [this] "Remove the deducer, returning the contained value"))

(defprotocol Accumulator
  "An accumulator tells how to fold two values together. This can be viewed
  like a fn passable to reduce, which takes two values and accumulates one
  into the other. The fn \"accum\" takes two values of the same type and returns
  the same type, with the two args combined/folded together as the result."
  (accum [this x] "Accumulates this and x: see the Accumulator protocol"))

(defn deduce-all
  "Applies the deducing function f to the args (in \"parallel\") and combines them.
  The args should all be the of the same type, and it should extend the ApplySpecial
  and MultiDeducer protocols."
  [f arg & args]
  (apply-all arg f args))

(defn deduce-all-with
  "Like deduce-all, but wraps the dispatching type (the first arg) with the given
  deducer constructor fn, and calls unwrap at the end. Sugar for using AdhocDeducer types."
  [d f arg & args]
  (unwrap (apply deduce-all f (d arg) args)))

(defprotocol ^:private Binding
             "Dispatch >>= polymorphically"
             (bind
               [seq-or-obj v]
               [seq-or-obj v d]))

(extend-protocol Binding
  Object
  (bind [mf mv]
    (deduce mv mf))
  clojure.lang.Seqable
  (bind [[mf & mfs] v]
    (if mf
      (recur mfs (bind mf v))
      v)))

(defn- deduce* [mv mf]
  (-> mv (apply-to mf) handle-nested))

(defn >>=
  "Iteratively deduce the value mv through the function(s) mfs."
  [mv & mfs]
  (bind mfs mv))

(defn deducer->>=
  "Like >>= for Adhoc (or any other deducer you want to wrap with). Instead of
  wrapping the init value with your deducer constructor,
  you can use this function instead of >>=, with your constructing fn passed in as
  an arg, and the value will be sent to the constructor.
  The constructor should take one arg, and construct a deducer with it, like the
  one returned from '->deducer.'

  If the deducer implements ValueDeducer, the unwrapped value will be returned; this
  is mostly useful for AdhocDeducer, which tries to be invisible.

  This function reads like, \"Use 'value' in a 'deducer' and pass it through 'fs'\""
  [value deducer & fs]
  (-> (deducer value) (>>= fs) unwrap))

(defn- process [f args [[v b] & more] body]
  `(~f ~b ~@args
      (fn [~v]
        ~(if more
           (process f args more body)
           body))))

(defn- deduce-form [defs body f & args]
  (process f args (partition 2 defs) (cons `do body)))

(defmacro let-deduce
  "A macro that allows deduced expressions to see the values of
  previously deduced expressions, similar to a let block.
  Each rhv of the binding vector 'defs' should return a deducer;
  e.g. (->Just ...), (MyDeducer. ...) - or ((->deducer ...) ...), for an ad-hoc.
  The body should also have a deducer in the final expression, so that you
  can chain the let-duduce with other deducing expressions"
  [defs & body]
  (deduce-form defs body `>>=))

(defmacro deduce-with
  "Similar to let-deduce in structure, with an added deducer arg. This arg should
  be a fn that takes one arg and returns a deducer, like ->deducer creates.
  This \"constructor\" will wrap all rh expressions in the 'defs' binding vector, and
  in the body expression of the deduce-with."
  [deducer defs & body]
  (deduce-form defs body `deducer->>= deducer))

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

;; Extensions on clojure.core

(extend-type nil
  Maybe
  (maybe? [_])
  ApplySpecial
  (apply-to [this f])
  FlexDeducer
  (handle-nested [_])
  SimpleDeducer
  (deduce [_ _])
  ValueDeducer
  (unwrap [_] nil))

(extend Object
  Maybe
  {:maybe? identity}
  SimpleDeducer
  {:deduce deduce*}
  ApplySpecial
  {:apply-to (fn [o f] (Just. (f o)))}
  ValueDeducer
  {:unwrap identity})

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

(extend-type clojure.lang.IPersistentMap
  ApplySpecial
  (apply-to [this f]
    (into {} (map (fn [[k v]] [k (f v)])) this))
  ValueDeducer
  (unwrap [this] (:value this)))

(extend-type String
  ApplySpecial
  (apply-to [s f] (pr-str (f (clojure.edn/read-string s))))
  FlexDeducer
  (handle-nested [s] (clojure.edn/read-string s))
  Accumulator
  (accum [this x] (str this x)))

;; Ad-hoc

(defrecord AdHoc [apply-to handle-nested deduce apply-all value]
  ApplySpecial
  (apply-to [this f]
    (update this :value apply-to f))
  FlexDeducer
  (handle-nested [this]
    (update this :value handle-nested))
  SimpleDeducer
  (deduce [this f]
    (if deduce
      (update this :value deduce f)
      (deduce* this f)))
  MultiDeducer
  (apply-all [this f more]
    (update this :value apply-all f more)))

(defn ->deducer
  "Returns a fn that builds an ad-hoc deducer specified by the args:
   Either a fn or a map should be given (tested as counted?)
   A fn as arg acts as the deduce function in a SimpleDeducer.
  Otherwise, with a map arg, keys should be either :deduce or
   :apply-special and :handle-nested"
  [deducer-fns]
  (let [adhoc-fns (if (counted? deducer-fns)
                    deducer-fns
                    {:deduce deducer-fns})]
    #(map->AdHoc (assoc adhoc-fns :value %))))

;; "Composing" deducers

(defn compose
  "Compose nested deducing fns. If you have some deducer Foo nested inside a Bar,
  your deducing fn should not look like
  (-> nested-arg op-on-nested ->Foo ->Bar) ;; BAD!
  but instead you'd want to
  (compose ->Bar (-> nested-arg op-on-nested ->Foo))"
  [& fns]
  (let [[inner next & outer] (reverse fns)]
    (loop [f1 inner
           f2 next
           more outer]
      (let [composed
            (comp f2 #(deduce % f1))]
        (if-let [[next-fn & more-fns] (seq more)]
          (recur composed next-fn more-fns)
          composed)))))

;; Writer

(defrecord Acc [acc value]
  ApplySpecial
  (apply-to [_ func] (Acc. acc (func value)))
  FlexDeducer
  (handle-nested [_]
    (assoc value :acc (accum acc (:acc value)))))

(defrecord FlexWriter [acc value writer]
  SimpleDeducer
  (deduce [this f]
    (if-let [[val & writes] (f value)]
      (-> this
          (assoc :value val)
          (update :acc accum writes))))
  ValueDeducer
  (unwrap [_]
    (binding [*out* writer]
      (reduce #(apply println %2) nil (:value acc)))
    (.close writer)
    value))

;; Reader

(defrecord Reader [val env]
  SimpleDeducer
  (deduce [_ f]
    (if-let [result (f val env)]
      (Reader. result env)))
  ValueDeducer
  (unwrap [_] val))

(defmacro ArityReader [x]
  (let [args (repeatedly x gensym)]
    `(defrecord ~(symbol (str "Reader-" x)) [~'value ~@args]
       SimpleDeducer
       (deduce [this# f#]
         (update this# ~':value f# ~@args))
       ValueDeducer
       (unwrap [_] ~'value))))

;; Miscellanea

(defmacro let-safe
  "Like let, except wraps all expressions that are in the binding form in a
  call to 'maybe', making the expressions - and the let as a whole - nil-safe"
  [bindings & forms]
  (let [maybe (fn [e] `(maybe ~e))
        vars (take-nth 2 bindings)
        exprs (->> (rest bindings)
                   (take-nth 2)
                   (map maybe))]
    `(let-deduce ~(interleave vars exprs) ~@forms)))
