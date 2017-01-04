(ns deducers.core
  (:refer-clojure :exclude [map]))

;; rename: unit, pure, monad; join? maybe that's OK
;; deduce instead of >>=, =>, join, deducer->, in->, with->, via->
;; with renamed to deduce?

(defmacro ^:private definvokers [highest-arity]
  (let [args (repeatedly gensym)]
    `(defn ~'invoker [n#]
       (case n#
         0 (fn [f# v# _#] (f# v#))
         ~@(apply concat
                  (for [x (range 1 (inc highest-arity))
                        :let [argv (vec (take x args))]]
                    `[~x (fn [f# v# ~argv]
                           (f# v# ~@argv))]))
         (fn [f# v# args#] (apply f# v# args#))))))

(definvokers 25)

(defmacro multidef [val & names]
  `(do ~@(clojure.core/map (fn [name#]
                             `(def ~name# ~val)) names)))

(defn apply*
  "Can be used in a Deducer's map* implementation to update its value"
  [mv f args]
  ((-> args
       count
       invoker)
   f mv args))

;; FUNCTOR
(defprotocol Mapping
  "Maps a special value against a bare function"
  (map* [this f args] "returns the resulting value in the same context"))

(defn map [f v & m]
  (map* v f m))

(defn invoke
  "Invokes the function f with deducer v and args"
  [f v & args]
  (map* v f args))

;; invoke* - applicative protocol
;; apply* - helper
;; ? apply-to invoke-on invoke-with invoke-in apply-in apply-with
;; update* bind*

;; return and pure are equivalent, not unit
;; unit is ()
;; mzero and mplus are MonadPlus, corresponding to
;; mempty and mappend in Monoid; mconcat is just mappend in reduce

(defprotocol Contextual
  "Details how to invoke a fn in a context against a value in a context, and how to put
  a bare value in context"
  (constructor [_] "Returns a fn that puts a bare value in a minimal context")
  (merge* [v]))

(defn partial*
  "Creates a partial function inside a deducer context
  f is the function to put in context
  c is the constructor for the deducer"
  [c f & args]
  (c (apply partial f args)))

(defn- invoke-with [f p x args]
  (merge* (map #(map* x (f %) args) p)))

(defn invoke-deducer
  "invokes the partial-deducer with an optional final arg
  p is the partial deducer, x is the deducer arg"
  ([p]
   (map #(%) p))
  ([p x & args]
   (invoke-with identity p x args)))

(defn add-partial-args
  "adds an arg to a partial-deducer
  p is the partial-in-context
  x is the deducer arg to add to the partial"
  [p x & args]
  (invoke-with (partial partial partial) p x args))

(defn <*> [f & mvs]
  (loop [[v & vs] mvs
         d (partial* (constructor v) f)]
    (if (seq vs)
      (recur vs (add-partial-arg d v))
      (invoke-deducer d v))))

;; MONAD
(defprotocol Deducer
  "Needs to also implment Functor"
  (join [this]))

(def ^:dynamic return identity)

(defn >>=
  "Passes the deducer through the function specifications
  mf should be a sequence of mf followed by args"
  ([mv [mf & args]]
   (map* mv mf args))
  ([mv mf & mfs]
   (reduce >>= (>>= mv mf) mfs)))

(defmacro =>
  "Like ->, but for deducers"
  [mv & mfs]
  `(>>= ~mv ~@(map #(vec %) mfs)))

(defmacro with [binding-defs & body]
  (letfn [(build-form [bindings]
            (if-let [[name expr & more] (seq bindings)]
              `(join (invoke (fn [~name]
                               ~@(build-form more)) ~expr))
              body))]
    (build-form binding-defs)))

;; (defn invoke* [mv mf]
;;   (binding [return (pure mv)]
;;     (join (invoke mf mv))))

;; ;; MONAD with return instead of map*
;; (defprotocol Deducer
;;   "Needs to also implement New"
;;   (bind ))

;; (defn return)
;; (defn unit)

;; (defprotocol Applicative
;;   (invoke*))
;; (defn <*>)

;; (defprotocol Zonoid
;;   (empty))

;; (declare mempty mzero)

;; (defprotocol Monoid
;;   plus)

;; (declare mplus <|> mappend)

;; (defprotocol Monoid+
;;   (combine))

;; (declare mconcat combine)

;; Implementations
(extend-type nil
  Mapping
  (map* [_ _ _])
  Deducer
  (join [_]))

(extend-type Object
  Mapping
  (map* [this f args]
    (apply f this args))
  Deducer
  (join [this] this))

;; java.util.Collection clojure.lang.Sequential Seqable IPersistentCollection Counted(?)
(extend-type clojure.lang.IPersistentCollection ;; implements empty
  Contextual
  (map* [this f args]
    (reduce #(conj %1 (f %2)) (empty this) this)))
