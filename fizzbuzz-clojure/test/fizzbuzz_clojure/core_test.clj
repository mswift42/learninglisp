(ns fizzbuzz-clojure.core-test
  (:require [clojure.test :refer :all]
            [fizzbuzz-clojure.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))


(deftest test-fizz
  (is (= (fizz 4) 4))
  (is (= (fizz 3) "Fizz"))
  (is (= (fizz 5) "Buzz"))
  (is (= (fizz 15) "FizzBuzz")))


(deftest test-fizz-extended
  (is (= (fizzbuzz-2 13) "Fizz"))
  (is (= (fizzbuzz-2 35) "FizzBuzz"))
  (is (= (fizzbuzz-2 22) 22)))
