(ns fizzbuzz-clojure.core)

(defn fizz [n]
  (cond
   (and (= 0 (mod n 3))
        (= 0 (mod n 5))) "FizzBuzz"
   (= 0 (mod n 3)) "Fizz"
   (= 0 (mod n 5)) "Buzz"
   :else n))

(defn fizzbuzz-simple
  "simple implementation for fizzbuzz"
  []
  (dotimes [n 100] (println (fizz (inc n)))))

(defn int-to-digits
  [n]
  (for [character (str n)] (Integer/parseInt (str character))))

(defn fizzbuzz-2
  "additionally return Fizz for every number containing a 3"
  [n]
  (cond
   (and (some #(= 3 %) (int-to-digits n))
        (= 0 (mod n 5))) "FizzBuzz"
   (some #(= 3 %) (int-to-digits n)) "Fizz"
   :else (fizz n)))

(defn fizzbuzz-extended
  "loop over 1 to 100 print fizzbuzz-2"
  []
  (dotimes [n 100] (println (fizzbuzz-2 (inc n)))))


