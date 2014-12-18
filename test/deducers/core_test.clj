(ns deducers.core-test
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test :as test]
            [deducers.core :refer :all]))

(clojure.test.check.clojure-test/defspec functor-test 100
  (prop/for-all [v (gen/vector gen/int)]
    (let [result (fmap v inc)]
      (= result (map inc v))
      (vector? result))))

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

(defspec test-functor-vector 100
  (prop/for-all [v (gen/vector gen/int)]
    (functor? v inc #(* 2 %))))

(defspec test-functor-list 100
  (prop/for-all [l (gen/list gen/int)]
    (functor? l inc #(* 2 %))))

(defspec test-functor-map 100
  (prop/for-all [m (gen/map gen/keyword gen/int)]
    (functor? m inc #(* 2 %))))

(defspec test-functor-just 100
  (prop/for-all [m gen/int]
    (functor? (->Just m) inc #(* 2 %))))

(test/deftest test-functor-nil
  (test/is (functor? nil inc #(* 2 %))))

(defspec test-functor-fn 100
  (let [*3 #(* 3 %)
        plus-100 #(+ 100 %)
        fun (fmap plus-100 *3)]
    (prop/for-all [i gen/int]
       (= (fun i) ((comp *3 plus-100) i)))))

(defspec test-functor-seq 100
  (prop/for-all [m (gen/list gen/int)]
    (functor? (map inc m) inc #(* 2 %))))

(defspec test-functor-queue 100
  (prop/for-all [m (gen/list gen/int)]
    (functor? (into clojure.lang.PersistentQueue/EMPTY m) inc #(* 2 %))))
