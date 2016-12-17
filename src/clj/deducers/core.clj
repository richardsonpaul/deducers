(ns deducers.core
  (:refer-clojure :exclude [map]))

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
  `(do ~@(map (fn [name#]
                `(def ~name# ~val)) names)))

;; FUNCTOR
(defprotocol Contextual
  (constructor [_])
  (map* [this f args]))
(multidef constructor unit return pure)

(defn apply*
  "Can be used in a Deducer's map* implementation to update its value"
  [mv f args]
  ((-> args
       count
       invoker)
   f mv args))

(defn map [f m]
  (map* m f []))

(defn- invoke* [mv [mf & args]]
  (map* mv mf args))

(defn invoke
  "Invokes the function f with deducer v and args"
  [f v & args]
  (invoke* v (cons f args)))

;; MONAD
(defprotocol Deducer
  "Needs to also implment Functor"
  (join [this]))

(def ^:dynamic return identity)

(defn >>=
  "Passes the deducer through the function specifications
  mf should be a sequence of mf followed by args"
  [mv & mfs]
  (binding [return (pure mv)]
    (reduce invoke* mv mfs)))

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

(defrecord Writer [value log]
  Unit (pure [_] (Writer. nil nil))
  Functor (map* [this f args] (update this :value apply* f args))
  Monad (join [this] (update value :log into log)))

(defn log [x] (->Writer nil [x]))

(defn log-and-return [l v]
  (->Writer v (list l)))

(defrecord Tracer [x]
  Unit (pure [_] ->Tracer)
  Functor (map* [this f args]
            (apply println "Calling" f "with" x args)
            (update this :x apply* f args))
  Monad (join [this] x))

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
  Unit
  (pure [_])
  Functor
  (map* [_ _ _])
  Monad
  (join [_]))

(extend-type Object
  Unit
  (pure [_] identity)
  Functor
  (map* [this f args]
    (apply f this args))
  Monad
  (join [this] this))

;; java.util.Collection clojure.lang.Sequential Seqable IPersistentCollection Counted(?)
(extend-type clojure.lang.IPersistentCollection ;; implements empty
  Functor
  (map* [this f args]
    (reduce #(conj %1 (f %2)) (empty this) this)))
