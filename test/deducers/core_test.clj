(ns deducers.core-test
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [deducers.core :refer :all]))


(clojure.test.check.clojure-test/defspec functor-test 100
  (prop/for-all [v (gen/vector gen/int)]
    (let [result (fmap v inc)]
      (= result (map inc v))
      (vector? result))))
