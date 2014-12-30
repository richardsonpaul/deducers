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

(test/deftest test-deduce-with-acc
  (let [start "Started with 3, "
        middle "incremented, "
        end "then added 2!"]
    (test/is (= (->Acc (str start middle end) 6)
                (let-deduce [x (->Acc start 3)
                             y (->Acc middle (inc x))]
                         (->Acc end (+ y 2)))))))

(test/deftest test-safe-let
  (let [let-test #(let-safe [x % y (+ x 4)] (* x y))]
    (test/is (= 21 (let-test 3)))
    (test/is (nil? (let-test nil)))))

(test/deftest test-implicit-context
  (test/is (= [[3 0 0] [3 1 0] [3 1 1] [3 2 0] [3 2 1] [3 2 2]]
              (let-deduce [x 3 y (range x) z (range (inc y))] (list [x y z])))))

(test/deftest test-vector-context
  (test/is (= [[3 0 0] [3 1 0] [3 1 1] [3 2 0] [3 2 1] [3 2 2]]
              (let-deduce [x [3] y (range x) z (range (inc y))] (list [x y z])))))

(test/deftest test-adhoc
  (let [events [{:damage 3}
                {:heal 4}
                {:damage 2}
                {:foo :bar}
                {:super-heal 2}]
        guy {:health 10}
        process (fn [event-type merge-fn]
                  (fn [events]
                    (let [amount (-> (comp (filter event-type) (map event-type))
                                     (transduce + events))]
                          [{:merge-fn merge-fn :health amount}
                           (remove event-type events)])))
        damage (process :damage -)
        heal (process :heal +)
        super-heal (process :super-heal *)
        apply-to (fn [[c e] f]
                   [c (f e)])
        handle-nested (fn [[og [{:keys [merge-fn health]} events]]]
                        [(merge-with merge-fn og {:health health}) events])
        adhoc-gen {:apply-to apply-to :handle-nested handle-nested}
        deduce (fn [[c e] f]
                 (let [[m new-e] (f e)
                       {:keys [merge-fn health]} m]
                   [(merge-with merge-fn c {:health health}) new-e]))
        expected [{:health 18} [{:foo :bar}]]]
    (test/is (= expected
                (-> [guy events]
                    (deducer->>= (->deducer adhoc-gen) damage heal super-heal))))
    (test/is (= expected
                (-> [guy events]
                    (deducer->>= (->deducer deduce) damage heal super-heal))))))

(test/deftest test-deduce-with
  (letfn [(f [[x] f] (apply vector x (f x)))]
    (test/is
     (= [3 4 12]
        (deduce-with (->deducer f)
                     [x [3]
                      y (-> x inc vector)]
                     [(* x y)])))))
