(ns cluster.core
  (:gen-class)
  (:require [clojure.string :as str]))

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

(defn parse-file
  [filepath]
  (with-open [reader (clojure.java.io/reader filepath)]
    (doall
      (map
        (fn [line]
          {:pos (vec
            (map
              #(Double/parseDouble %)
              (butlast (str/split line #","))))
          :pot 0.0})
        (line-seq reader)))))

(defn -main
  [filepath distance-name]
  (println (parse-file filepath)))
