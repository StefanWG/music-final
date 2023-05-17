(ns music.nn
  (:require [clojure.test :refer :all]
            [clj-synapses.net :as net]
            [clj-synapses.fun :as fun])
  (:import (java.util Random)))


;; model = Sequential ()
;; model.add (Dense (512, activation='relu', input_dim=3))
;; model.add (Dense (512, activation='relu'))
;; model.add (Dense (1, activation='sigmoid'))
;; model.compile (optimizer='adam', loss='binary_crossentropy', metrics= ['accuracy'])

(defn readBachDataset []
  (map read-string (clojure.string/split-lines (slurp "melodies.txt"))))

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
(def network
  (net/json->
    "[[{\"activationF\" : \"leaky-re-lu\", \"weights\" : [-0.5,0.1,0.8]},
       {\"activationF\" : \"leaky-re-lu\", \"weights\" : [0.7,0.6,-0.1]},
      [{\"activationF\" : \"sigmoid\", \"weights\" : [0.5,-0.3,-0.4,-0.5]}]]"))

(net/->svg
 custom-network)

(net/predict
  network
  [0.2 0.6])