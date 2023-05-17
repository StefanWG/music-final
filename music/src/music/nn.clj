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

(defn changeFormat [ind]
  [(vec (map (fn [g] (:note g)) (:genome ind))) [(:feedback ind)]])

(defn readBachDataset []
  (let [melodies (map read-string (clojure.string/split-lines (slurp "melodies.txt")))]
    (vec (map changeFormat melodies))))

(defn inputNum
  "Finds the input dimension, which is the maximum number of notes in melody from bach dataset"
  [bach]
  (loop [melody bach
         numNotes []]
    (if (empty? melody)
      (max numNotes)
      (recur (rest melody) (conj numNotes (count (filter #(:note %) (first melody))))))))

(def network
  (net/->net
   [(inputNum (readBachDataset)) (- (inputNum (readBachDataset)) (/ (inputNum (readBachDataset)) 2)) 3]))

;; How do I use leaky-re-lu?
;; What should the weights be? 

(net/->net
    [(inputNum (readBachDataset)) (- (inputNum (readBachDataset)) (/ (inputNum (readBachDataset)) 2)) 3]
    (fn [_] fun/leaky-re-lu)
    (fn [_] (rand)))

;; (def network
;;   (net/json->
;;     "[[{\"activationF\" : \"leaky-re-lu\", \"weights\" : [-0.5,0.1,0.8]},
;;        {\"activationF\" : \"leaky-re-lu\", \"weights\" : [0.7,0.6,-0.1]},
;;       [{\"activationF\" : \"sigmoid\", \"weights\" : [0.5,-0.3,-0.4,-0.5]}]]"))

(reduce
 (fn [acc [xs ys]]
   (net/fit acc 0.1 xs ys))
 net
 [[[0.2 0.6] [0.9]]
  [[0.1 0.8] [0.2]]
  [[0.5 0.4] [0.6]]])

;; (net/fit
;;  net
;;  0.1
;;  [0.2 0.6]
;;  [0.9])

(net/->svg
 custom-network)

(net/predict
  network
  [0.2 0.6]) ;;has to predict from a melody we feed it into