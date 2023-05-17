(ns music.nn
  (:require [clojure.test :refer :all]
            [clj-synapses.net :as net]
            [clj-synapses.fun :as fun])
  (:import (java.util Random)))


;; PYTHON:
;; model = Sequential ()
;; model.add (Dense (512, activation='relu', input_dim=3))
;; model.add (Dense (512, activation='relu'))
;; model.add (Dense (1, activation='sigmoid'))
;; model.compile (optimizer='adam', loss='binary_crossentropy', metrics= ['accuracy'])

(defn changeFormat [ind maxLen]
  (let [notes  (vec (map (fn [g] (:note g)) (:genome ind)))
        feed [(:feedback ind)]]
    [(into notes (repeat (- maxLen (count notes)) 0)) feed]))

(defn readBachDataset []
  (let [melodies (map read-string (clojure.string/split-lines (slurp "melodies.txt")))
        maxLen (apply max (map #(count (:genome %)) melodies))
        formatted (vec (map #(changeFormat % maxLen) melodies))]
    formatted))

(defn splitBach [bach n]
  (loop [zeroes []
         ones []
         twos []
         bachLeft bach]
    (if (= 0 (count bachLeft))
      (into (into (take n zeroes) (take n ones)) (take n twos))
      (let [b (first bachLeft)
            f (nth (nth b 1) 0)]
        (cond
          (= 0 f) (recur (conj zeroes b) ones twos (rest bachLeft))
          (= 1 f) (recur zeroes (conj ones b) twos (rest bachLeft))
          (= 2 f) (recur zeroes ones (conj twos b) (rest bachLeft)))))))

(defn bachOfLengthN [n]
  (let [melodies (map read-string (clojure.string/split-lines (slurp "melodies.txt")))
        formatted (vec (map #(changeFormat % (count %)) melodies))]
    (filter #(= n (count (first %))) formatted)))
(map read-string (clojure.string/split-lines (slurp "melodies.txt")))

(defn inputNum
  "Finds the input dimension, which is the maximum number of notes in melody from bach dataset"
  [bach]
  (loop [melody bach
         numNotes []]
    (if (empty? melody)
      (apply max numNotes)
      (recur (rest melody) (conj numNotes (count (nth (first melody) 0)))))))

(defn getNN [fp]
  (let [bach (splitBach (readBachDataset) 3333)
        shuffled (shuffle bach)
        network (net/->net
                 [32 32 1])]
    (do 
      (reduce
       (fn [acc [xs ys]]
         (net/fit acc 0.2 xs ys))
       network
       shuffled)
      (spit fp (net/->json 
       network)))))

;; ;;TESTING NEURAL NET
;; (def bach (splitBach (readBachDataset) 3333))
;; (def shuffled (shuffle bach))
;; (def training (take (* (/ (count bach) 3) 2) shuffled))
;; (def testing-d (take-last (/ (count bach) 2) shuffled))

;; (def network
;;   (net/->net
;;    [(inputNum bach) (inputNum bach) 1]))

;; (reduce
;;  (fn [acc [xs ys]]
;;    (net/fit acc 0.2 xs ys))
;;  network
;;  training)

;; (map (fn [mel] [(net/predict network (first mel)) (second mel)]) testing-d)
;; ;; AVG absolute error
;; (/ (reduce + (map #(abs (- (first %) (second %)))
;;                   (map (fn [mel]
;;                          [(first (net/predict network (nth mel 0))) (first (nth mel 1))])
;;                        testing-d))) (count testing-d))
;; ;; AVG squared error
;; (/ (reduce + (map #(* (- (first %) (second %)) (- (first %) (second %)))
;;                   (map (fn [mel]
;;                          [(first (net/predict network (nth mel 0))) (first (nth mel 1))])
;;                        testing-d))) (count testing-d))

;; (readBachDataset)