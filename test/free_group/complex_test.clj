(ns free-group.complex-test
  (:require [clojure.test :refer :all]
            [free-group.complex :refer :all]
            [quil.core :as q]
            [quil.middleware :as m]))

(deftest complex-test
  (testing "Complex cast fail."
    (is (= ({:real 1.0 :complex 0.0} (complex 1.0))))
    (is (= ({:real 1.0 :complex 2.0} (complex 1.0 2.0))))
    ))

(deftest complex-polar-test
  (testing "Complex polar fail."
    (let [pi 3.14159265358979323846264338327950
          epsilon 0.0000001]
      (is (< (complex-abs (complex+ (complex-polar 1.0 pi) (complex 1.0))) epsilon))
      (is (< (complex-abs (complex+ (complex-polar 2.0 (/ pi 2.0)) (complex 0.0 -2.0))) epsilon)))
    ))

(deftest complex*-test
  (testing "Complex multiplication fail."
    (is (= (complex* (complex 1.0) (complex 2.0)) (complex 2.0)))
    (is (= (complex* (complex 2.0 -2.0) (complex 2.0 2.0)) (complex 8.0)))
    ))

(deftest complex+-test
  (testing "Complex addition fail."
    (is (= (complex+ (complex 1.0) (complex 2.0)) (complex 3.0)))
    (is (= (complex+ (complex 2.0 -2.0) (complex 2.0 2.0)) (complex 4.0)))
    ))

(deftest complex-conj-test
  (testing "Complex conjugation fail."
    (is (= (complex-conj (complex 1.0 2.0)) (complex 1.0 -2.0)))
    ))

(deftest complex-div-test
  (testing "Complex division fail."
    (is (= (complex-div (complex 1.0 1.0) (complex -1.0 -1.0)) (complex -1.0)))
    (is (= (complex-div (complex 3.0 1.0) (complex 2.0 -1.0)) (complex 1.0 1.0)))
    ))

(deftest complex-abs-test
  (testing "Complex abs fail."
    (is (= (complex-abs (complex 3.0 4.0)) 5.0))
    ))


