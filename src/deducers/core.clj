(ns deducers.core)

;; FUNCTOR
(defprotocol Functor
  (fmap [this f args]))

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
  "Can be used in a Deducer's fmap implementation to update its value"
  [mv f args]
  ((-> args
       count
       invoker)
   f mv args))

(defn- invoke* [mv [mf & args]]
  (fmap mv mf args))

(defn invoke
  "Invokes the function f with deducer v and args"
  [f v & args]
  (invoke* v (cons f args)))

(defn >>=
  "Passes the deducer through the function specifications
  mf should be a sequence of mf followed by args"
  [mv & mfs]
  (binding [return (pure mv)]
    (reduce invoke* mv mfs)))

(defmacro =>
  "Like ->, but using for deducers"
  [mv & mfs]
  `(>>= ~mv ~@(map #(vec %) mfs)))

;; MONAD
(defprotocol Monad
  "Needs to also implment Functor"
  (join [this]))

(def ^:dynamic return identity)

(defmacro with [bindings & body]
  (letfn [(binding-form [form-fn name expr]
            (fn [inner-form]
              (form-fn
               `(join (invoke (fn [~name] ~inner-form) ~expr)))))
          (fn-builder [form-fn bindings]
            (if-let [[name expr & rest] (seq bindings)]
              (recur (binding-form form-fn name expr) rest)
              form-fn))]
    ((fn-builder identity bindings) (cons `do body))))

;; (defn invoke* [mv mf]
;;   (binding [return (pure mv)]
;;     (join (invoke mf mv))))

;; ;; MONAD with return instead of fmap
;; (defprotocol Deducer
;;   "Needs to also implement New"
;;   (bind ))

;; FUNCTOR/APPLICATIVE/MONAD
(defprotocol Unit
  (pure [_]))

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
  Functor
  (fmap [_ _ _])
  Monad
  (join [_]))

(extend-type Object
  Functor
  (fmap [this f args]
    (apply f this args))
  Monad
  (join [this] this))
