(ns cluster.core-test
  (:require [clojure.test :refer :all]
            [cluster.core :refer :all]))

(deftest test-initialize-potentials
  (is (=
    (initialize-potentials '([12.32 4.23 6.42] [11.41 5.12 2.46]) distance-func-euclidean)
    '({:pos [12.32 4.23 6.42], :pot 1.069800948794656} {:pos [11.41 5.12 2.46], :pot 1.069800948794656}))))

(deftest test-distance-func-hamming
  (is (=
    (distance-func-hamming [12 21 43 55 23] [12 25 43 54 23])
    2)))

(deftest test-distance-func-euclidean
  (is (=
    (distance-func-euclidean [11 7] [8 3])
    5.0)))
