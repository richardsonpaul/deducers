(ns deducers.core
  (:refer-clojure :exclude [map ->]))

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
  ((clojure.core/-> args
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
  "dealing with a value in context"
  (constructor [_] "Returns a fn that puts a bare value in a minimal context")
  (join [this] "joins a nested context with its parent (\"this\"), un-nesting it")
  (fold [this other accept] "if the other value can be joined with this, call accept;
      if not, can return this or nil, as appropriate"))

(defrecord Invocation [f d vs]
  Contextual
  (map* [this f _] (f this)))

(defn invoke [f & [v & vs]]
  (let [invoker-arg (fn [i a]
                      (map #(update i :vs conj %) a))
        add-arg (fn [d a]
                  (map #(invoker-arg % a) d))
        insert-arg (comp join add-arg)
        initial (map #(->Invocation f % []) v)
        execute-invocation (fn [x] (map* (:d x) (:f x) (:vs x)))]
    (map execute-invocation (reduce #(fold %1 %2 insert-arg) initial vs))))
;; todo: make "accept" be a thunk, instead of the deducer calling it with itself and v
;; should it do any work? e.g. return a merged/joined "this" and a bare val? Might be faster, but more annoying to implement;
;; plus then they'll have to call it properly and everything. The way I have it is easy.

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
(def nf ->NumberFilter)
;; for writer, we output a Logger. It concats strings, and then on a special call, outputs them - or on join?
;; for reader, we use a Input. It has some strings, and gives them to the function (how?)
;; on a special call, it will read its stuff from stdin. But ... reverse?
;; When constructed, it can read a stream on a special call (or be built with explicit strings)
;; Then, on a call, it... does a binding around the fn?
;; Receives a fn as a result of something that wants to read, and puts the strings into it?

(def ^:dynamic return identity)

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

;; rename to deduce, or?
;; make it explicit there's a deducer context? Just note it in the doc, that all rhs of bindings must be the same deducer?
(defmacro with [binding-defs & body]
  (letfn [(build-form [bindings]
            (if-let [[name expr & more] (seq bindings)]
              `(join (map (fn [~name]
                            ~@(build-form more)) ~expr))
              body))]
    `(join ~(build-form binding-defs))))
;; do a final join; make a comp of join & map?

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
  (fold [this v k]
    (if v
      (k this v)
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

;; aliases to haskell names
(def fmap map)
(def <*> invoke)
(def ^:macro deducer-> #'with->)
(def ^:macro >>= #'deducer->)
(def ^:macro -> #'deducer->)
