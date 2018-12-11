(ns free-group.core-test
  (:require [clojure.test :refer :all]
            [free-group.core :refer :all]
            [free-group.complex :refer :all]
            [quil.core :as q]
            [quil.middleware :as m]))

(deftest mobius-translation-test
  (testing "Mobius translation fail."
    (is (= ({:a (complex 1.0) :b (complex 2.0 1.0) :c (complex 0.0) :d (complex 1.0)} (mobius-translation (complex 2.0 1.0)))))
    ))

(deftest mobius-scaling-test
  (testing "Mobius scaling fail."
    (is (= ({:a (complex 2.0) :b (complex 0.0) :c (complex 0.0) :d (complex 1.0)} (mobius-scaling 2.0))))
    ))

(deftest mobius*-test
  (testing "Mobius multiplication fail."
    (is (= {:a (complex 3.0 8.0) :b (complex -2.0 6.0)
            :c (complex -2.0 2.0) :d (complex 0.0 6.0)}
           (mobius*
                    {:a (complex 1.0 1.0) :b (complex 2.0 1.0)
                     :c (complex -1.0) :d (complex 1.0)}
                    {:a (complex 3.0) :b (complex 0.0 -2.0)
                     :c (complex 1.0 2.0) :d (complex 0.0 4.0)})))
    (is (= {:a (complex -14.0 35.0) :b (complex 0.0 13.0)
            :c (complex -12.0 20.0) :d (complex -6.0 -4.0)}
           (mobius*
                    {:a (complex 1.0 1.0) :b (complex 2.0 1.0)
                     :c (complex -1.0) :d (complex 1.0)}
                    {:a (complex 3.0) :b (complex 0.0 -2.0)
                     :c (complex 1.0 2.0) :d (complex 0.0 4.0)}
                    {:a (complex 2.0 1.0) :b (complex 2.0 1.0)
                     :c (complex 3.0 1.0) :d (complex -1.0)})))
    (let [A {:a (complex 1.0 2.0) :b (complex 3.0 4.0)
             :c (complex 5.0 6.0) :d (complex 7.0 8.0)}
          B {:a (complex 2.0 3.0) :b (complex 4.0 5.0)
             :c (complex 6.0 7.0) :d (complex 8.0 9.0)}
          C {:a (complex 3.0 4.0) :b (complex 5.0 6.0)
             :c (complex 7.0 8.0) :d (complex 9.0)}]
      (is (= (mobius* A B C) (mobius* (mobius* A B) C) (mobius* A (mobius* B C)))))
    ))

(deftest normalize-homogenous-test
  (testing "Normalization of homogenous fail."
    (is (= {:z0 (complex 13.0 12.0) :z1 (complex 1.0)}
           (normalize-homogenous {:z0 (complex 26.0 24.0) :z1 (complex 2.0)})))
    (is (= {:z0 (complex 1.0) :z1 (complex 0.0)}
           (normalize-homogenous {:z0 (complex 2.0 3.0) :z1 (complex 0.0)})))
    ))

(deftest mobius-of-point-test
  (testing "Mobius of point fail."
    (is (= (mobius-of-point {:a (complex 1.0 2.0) :b (complex 2.0 1.0)
                             :c (complex 2.0 3.0) :d (complex 3.0 2.0)}
                            {:z0 (complex 2.0 1.0) :z1 (complex 2.0 3.0)})
           {:z0 (complex 1.0 13.0) :z1 (complex 1.0 21.0)}))
    ))

(deftest mobius-involution-test
  (testing "Mobius involution fail."
    (is (= (mobius-involution {:center (complex 0.0) :radius 1.0})
           {:a (complex 0.0) :b (complex 1.0)
            :c (complex 1.0) :d (complex 0.0)}))
    ))

(deftest involution-of-circle-test
  (testing "Involution of circle fail."
    (is (= (involution-of-circle {:center (complex 0.0) :radius 1.0}
                                 {:center (complex 0.0) :radius 2.0})
           {:center (complex 0.0) :radius 0.5 :color nil}))
    (is (= (involution-of-circle {:center (complex 0.0) :radius 1.0}
                                 {:center (complex 2.0) :radius 1.0})
           {:center (complex (/ 2.0 3.0)) :radius (/ 1.0 3.0) :color nil}))
    (is (= (involution-of-circle {:center (complex 0.0) :radius 1.0}
                                 {:center (complex 0.0 2.0) :radius 1.0})
           {:center (complex 0.0 (/ 2.0 3.0)) :radius (/ 1.0 3.0) :color nil}))
    ))

(deftest generate-words-test
  (testing "Word generation fail."
    (is (= (clojure.core/set (generate-words 2 2))
           #{'(0) '(1) '(0 1) '(1 0)}))
    (is (= (clojure.core/set (generate-words 3 1))
           #{'(0) '(1) '(2)}))
    ))

(deftest word-to-circle-test
  (testing "Word to circle fail."
    (let [A {:center (complex 0.0) :radius 1.0 :color nil}
          B {:center (complex 0.0) :radius 2.0 :color nil}]
      (is (= (word-to-circle [A B] '(0)) A))
      (is (= (word-to-circle [A B] '(1)) B))
      (is (= (word-to-circle [A B] '(0 1))
             {:center (complex 0.0) :radius 0.5 :color nil}))
      (is (= (word-to-circle [A B] '(1 0))
             {:center (complex 0.0) :radius 4.0 :color nil}))
      )))

(deftest words-to-circles-test
  (testing "Words to circles fail."
    (let [A {:center (complex 0.0) :radius 1.0 :color nil}
          B {:center (complex 0.0) :radius 2.0 :color nil}]
      (is (= (clojure.core/set (words-to-circles [A B] #{'(0) '(1) '(0 1) '(1 0)}))
             #{A B {:center (complex 0.0) :radius 0.5 :color nil} {:center (complex 0.0) :radius 4.0 :color nil}}))
      )))

(deftest lengthen-word-test
  (testing "Lengthen word fail."
    (is (= (clojure.core/set (lengthen-word 3 '(0)))
           #{'(1 0) '(2 0)}))
    ))

(deftest lengthen-words-test
  (testing "Lengthen words fail."
    (is (= (clojure.core/set (lengthen-words 2 (list (list 0) (list 1))))
           #{(list 0 1) (list 1 0)}))
    ))
