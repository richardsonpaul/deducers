(ns deducers.core-test
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test :as test]
            [deducers.core :refer :all]))

(clojure.test.check.clojure-test/defspec apply-special-test 100
  (prop/for-all [v (gen/vector gen/int)]
    (let [result (apply-to v inc)]
      (= result (map inc v))
      (vector? result))))

(defn apply-special?
  "Test an object to see if it adheres to the apply-special laws.
  f and g are functions that operate on the apply-special"
  [ftor f g]
  (let [id (apply-to ftor identity)
        composed (apply-to ftor (comp f g))
        nested (apply-to (apply-to ftor g) f)
        follows-laws? (and (= id (identity ftor)) (= composed nested))]
    (when-not follows-laws?
      (println "Identity apply-toped:" id)
      (println "Composed:" (str composed "; Nested:") nested))
    follows-laws?))

(defspec test-apply-special-vector 100
  (prop/for-all [v (gen/vector gen/int)]
    (apply-special? v inc #(* 2 %))))

(defspec test-apply-special-list 100
  (prop/for-all [l (gen/list gen/int)]
    (apply-special? l inc #(* 2 %))))

(defspec test-apply-special-map 100
  (prop/for-all [m (gen/map gen/keyword gen/int)]
    (apply-special? m inc #(* 2 %))))

(defspec test-apply-special-just 100
  (prop/for-all [m gen/int]
    (apply-special? (->Just m) inc #(* 2 %))))

(test/deftest test-apply-special-nil
  (test/is (apply-special? nil inc #(* 2 %))))

(defspec test-apply-special-fn 100
  (let [*3 #(* 3 %)
        plus-100 #(+ 100 %)
        fun (apply-to plus-100 *3)]
    (prop/for-all [i gen/int]
       (= (fun i) ((comp *3 plus-100) i)))))

(defspec test-apply-special-seq 100
  (prop/for-all [m (gen/list gen/int)]
    (apply-special? (map inc m) inc #(* 2 %))))

(defspec test-apply-special-queue 100
  (prop/for-all [m (gen/list gen/int)]
    (apply-special? (into clojure.lang.PersistentQueue/EMPTY m) inc #(* 2 %))))

(test/deftest test-deduce-with
  (let [log-str "Did something"
        log-something (partial ->Acc log-str)]
    (test/is (= (->Acc (clojure.string/join (repeat 3 log-str)) 6)
              (deduce-with log-something
                           [x 3
                            y (inc x)]
                           (+ y 2))))))

(test/deftest test-safe-let
  (let [let-test #(let-safe [x % y (+ x 4)] (* x y))]
    (test/is (= 21 (let-test 3)))
    (test/is (nil? (let-test nil)))))

(test/deftest test-implicit-context
  (test/is (= [[3 0 0] [3 1 0] [3 1 1] [3 2 0] [3 2 1] [3 2 2]]
              (deduce [x 3 y (range x) z (range (inc y))] [x y z]))))

(test/deftest test-adds-return)
