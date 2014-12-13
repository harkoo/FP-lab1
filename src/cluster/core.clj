(ns cluster.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn distance-func-hamming
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
    "hamming" distance-func-hamming
    "euclidean" distance-func-euclidean))

(defn parse-file
  [filepath]
  (with-open [reader (clojure.java.io/reader filepath)]
    (doall
      (map
        (fn [line]
          (vec
            (map
              #(Double/parseDouble %)
              (butlast (str/split line #",")))))
        (line-seq reader)))))

(def r-A 2.5)
(def r-B (* 1.5 r-A))

(defn initialize-potentials
  [points, distance-function]
  (let [coeff (/ 4 (Math/pow r-A 2))]
    (map
      (fn [point]
        {
          :pos point
          :pot (reduce
            (fn [potential near-point]
              (+
                (Math/pow Math/E (* (- coeff) (distance-function point near-point)))
                potential))
            0.0
            points)
        })
      points)))

(defn reduce-potentials [points basis distance-function]
  (let [coeff (/ 4 (Math/pow r-B 2))]
    (map
      (fn [point]
        {
          :pos (:pos point)
          :pot (-
            (:pot point)
            (*
              (:pot basis)
              (Math/pow Math/E (* (- coeff) (distance-function (:pos point) (:pos basis))))))
        })
      points)))

(def e-up 0.5)
(def e-down 0.15)

(defn estimate-cluster-centers
  [points centers distance-function init-center]
  (let [point-with-max-potential (apply max-key #(:pot %) points)
        init-center (or init-center point-with-max-potential)
        init-center-potential (:pot init-center)
        reduced-potential-points (reduce-potentials points point-with-max-potential distance-function)]
    (cond
      (> (:pot point-with-max-potential) (* e-up init-center-potential))
      (recur reduced-potential-points (conj centers point-with-max-potential) distance-function init-center)

      (< (:pot point-with-max-potential) (* e-down init-center-potential))
      centers

      (>= (+
        (/
          (Math/sqrt (apply min (map #(distance-function (:pos point-with-max-potential) (:pos %)) centers)))
          r-A)
        (/ (:pot point-with-max-potential) init-center-potential)) 1)
      (recur reduced-potential-points (conj centers point-with-max-potential) distance-function init-center)

      :else
      (recur reduced-potential-points centers distance-function init-center))))

(defn -main
  [filepath distance-name]
  (let [distance-function (choose-distance-func distance-name)
        init-points (initialize-potentials (parse-file filepath) distance-function)]
    (doall (map println (estimate-cluster-centers init-points '() distance-function nil)))))
