(ns deducers.core-test
  (:refer-clojure :exclude [map])
  (:require
   [deducers.core :refer :all]
   [clojure.test :as test :refer [deftest is]]))

;; To test some deducer functions
(defrecord ^:private NumberFilter [x y]
  Contextual
  (map* [this f args]
    (update this :x apply* f args))
  Deducer
  (join [this] (-> this
                   (update :y concat (:y x))
                   (update :x :x)))
  (fold [this v k]
    (if (-> v type (= NumberFilter))
      (k)
      this)))

(defn nf [x] (->NumberFilter x [(str x)]))

(deftest test-invoke "Test invoking some things"
  (let [expected (->NumberFilter 6 ["1" "2" "3"])]
    (is (nil? (invoke + nil)))
    (is (= 6
           (invoke + 1 2 3)))
    (is (nil? (invoke + 1 nil 2 3)))
    (is (= expected
           (invoke + (nf 1) (nf 2) (nf 3))))
    (is (= expected
           (invoke + (nf 1) nil (nf 2) (nf 3))))
    (is (= expected
           (invoke + nil nil (nf 1) (nf 2) (nf 3))))))

(deftest test-map "Test mapping some stuff"
  (let [expected (->NumberFilter 5 (map str [2 3]))]
    (is (= nil (map inc nil)))
    (is (= 7 (map inc 6)))
    (is (= (->NumberFilter 6 ["5"])
           (map inc (nf 5))))
    (let [expected [2 3 4]]
      (is (let [actual (map inc [1 2 3])]
            (and (= expected actual)
                 (vector? actual))))
      (is (let [actual (map inc (list 1 2 3))]
            (and (= expected actual)
                 (list? actual)))))))
