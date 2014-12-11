(ns cluster.core-test
  (:require [clojure.test :refer :all]
            [cluster.core :refer :all]))

(deftest test-distance-func-euclidean
  (is (=
    (distance-func-euclidean [11 7] [8 3])
    5.0)))

(deftest test-distance-hamming-func
  (is (=
    (distance-hamming-func [12 21 43 55 23] [12 25 43 54 23])
    2)))
