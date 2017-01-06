(ns deducers.core
  (:refer-clojure :exclude [map merge]))

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
(defprotocol Contextual
  "Maps a special value against a bare function"
  (map* [this f args] "returns the resulting value in the same context"))

(defn map [f v & m]
  (map* v f m))

;; return and pure are equivalent, not unit
;; unit is ()
;; mzero and mplus are MonadPlus, corresponding to
;; mempty and mappend in Monoid; mconcat is just mappend in reduce

(defprotocol Deducer
  "dealing with a value in context"
  (constructor [_] "Returns a fn that puts a bare value in a minimal context")
  (join [v] "joins a nested context with its parent, un-nesting it")
  (fold [this other accept] "if the other value can be folded with this, call accept
      if not, can return this or nil, as appropriate"))

(defrecord Invocation [f d vs])

(defn <*> [f & [v & vs]]
  (let [bind (fn [x a] (update x :vs conj a))
        bind-with (fn [a i]
                    (map #(bind i %) a))
        add-binding (fn [d v]
                      (map #(bind-with v %) d))
        insert-binding (comp join add-binding)
        initial (map #(->Invocation f % []) v)
        invoke (fn [x] (map* (:d x) (:f x) (:vs x)))]
    (map invoke (reduce #(fold %1 %2 insert-binding) initial vs))))

(defrecord NumberFilter[x]
  Contextual
  (map* [this f args]
    (update this :x apply* f args))
  Deducer
  (constructor [_] ->NumberFilter)
  (join [this] (update this :x :x))
  (fold [this v k]
    (if v
      (k this v)
      this)))

(def ^:dynamic return identity)

(defn >>=
  "Passes the deducer through the function specifications
  mf should be a sequence of mf followed by args"
  [mv mf & mfs]
  (letfn [(bind [mv [mf & args]]
            (join (map* mv mf args)))]
    (reduce bind (bind mv mf) mfs)))

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
  Contextual
  (map* [_ _ _])
  Deducer
  (join [_]))

(extend-type Object
  Contextual
  (map* [this f args]
    (apply f this args))
  Deducer
  (constructor [_] identity)
  (join [this] this)
  (folding [this v k]
    (if v
      (k v)
      this)))

(defn- map-coll [coll f args]
  (reduce #(conj %1 (apply* %2 f args)) (empty coll) coll))

;; java.util.Collection clojure.lang.Sequential Seqable IPersistentCollection Counted(?)
(extend-type clojure.lang.IPersistentVector
  Contextual
  (map* [this f args] (map-coll this f args)))

(extend-type clojure.lang.IPersistentCollection
  Contextual
  (map* [this f args] (map-coll (reverse this) f args)))
