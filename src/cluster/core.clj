(ns cluster.core
  (:gen-class))

(defn distance-hamming-func
  [pos1 pos2]
  (apply +
    (map
      (fn [a b] (if (= a b) 0 1))
      pos1
      pos2)))

(defn distance-func-euclidean
  [pos1 pos2]
  (Math/sqrt
    (apply +
      (map
        (fn [a b] (Math/pow (- a b) 2))
        pos1
        pos2))))

(defn choose-distance-func
  [distance-name]
  (case distance-name
    "hamming" distance-hamming-func
    "euclidean" distance-func-euclidean))

(defn -main
  [file distance-name])
