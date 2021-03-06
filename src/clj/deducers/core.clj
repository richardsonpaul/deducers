(ns deducers.core
  (:refer-clojure :exclude [map]))

;; implement >> *>
;; handle nil-first <*>
;; handle (invoke conj [1] 2)
;; monoids

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

(defn apply*
  "Can be used in a Deducer's map* implementation to update its value"
  [mv f args]
  ((-> args
       count
       invoker)
   f mv args))

;; FUNCTOR
(defprotocol Contextual
  "Maps a contextual value against an ordinary function, updating the value in the context"
  (map* [this f args] "returns the resulting value in the same context"))

(defn map [f v & m]
  (map* v f m))

;; return and pure are equivalent, not unit
;; unit is ()
;; mzero and mplus are MonadPlus, corresponding to
;; mempty and mappend in Monoid; mconcat is just mappend in reduce

(defprotocol Deducer
  "a value in special \"computational\" context"
  (join [this] "joins a nested context with its parent (\"this\"), un-nesting it")
  (fold [this other merge] "if the other value can be joined with this, call accept;
      if not, can return this or nil, as appropriate"))

(def bind (comp join map))

(defrecord ^:private Arglist [xs]
  Contextual
  (map* [this f args]
    (update this :xs apply* f args))
  )

(defn invoke [f & [v & vs]]
  (let [initial {:f f :args (map #(->Arglist [%]) v)}
        add-arg (fn [args x]
                  (map #(conj % x) args))
        add-arg-in (fn [argv dx]
                     (map #(add-arg argv %) dx))
        bind-arg (fn [dori dy]
                   (fold dori dy (partial bind add-arg-in)))
        insert-arg (fn [i x] (update i :args bind-arg x))
        evaluate (fn [{:keys [f args]}]
                   (map #(apply f (:xs %)) args))]
    (evaluate (reduce insert-arg initial vs))))

(defn deduce
  "Passes the deducer through the function specifications
  mf should be a sequence of mf followed by args"
  [mv & mfs]
  (letfn [(bind [mv [mf & args]]
            (join (map* mv mf args)))]
    (reduce bind mv mfs)))

(defmacro with->
  "Like ->, but for deducers"
  [mv & mfs]
  `(deduce ~mv ~@(map #(vec %) mfs)))

(defmacro with [binding-defs & body]
  (letfn [(build-form [bindings]
            (if-let [[name expr & more] (seq bindings)]
              `(join (map (fn [~name]
                            ~@(build-form more)) ~expr))
              body))]
    (cons `join (build-form binding-defs))))

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
  (join [_])
  (fold [_ v k]
    (when v (fold v nil k))))

(extend-type Object
  Contextual
  (map* [this f args]
    (apply f this args))
  Deducer
  (constructor [_] identity)
  (join [this] this)
  (fold [this v k] (when v (k))))

(defn- map-coll [coll f args]
  (reduce #(conj %1 (apply* %2 f args)) (empty coll) coll))

;; java.util.Collection clojure.lang.Sequential Seqable IPersistentCollection Counted(?)
(extend-type clojure.lang.IPersistentVector
  Contextual
  (map* [this f args] (map-coll this f args)))

(extend-type clojure.lang.IPersistentCollection
  Contextual
  (map* [this f args] (map-coll (reverse this) f args)))

;; aliases to haskell names
(def fmap map)
(def <*> invoke)
(def ^:macro >>= #'with->)

;; and other non-haskell aliases
(def ^:macro deducer-> #'with->)
(def ^:macro => #'deducer->)
