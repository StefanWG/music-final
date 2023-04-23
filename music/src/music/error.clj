(ns music.error)
(require '[music.core :refer :all])


(defn restError 
  "Returns the number of rests in the melody (more rests means more error)"
  [genome]
  ;;Could be easily modified to include the length of rests with a 
  ;;larger punishment for longer rests
  (let [numRests (count (filter #(= (:note %) -1) genome))]
    numRests))

(defn getDiffs 
  "Returns the size of the jumps in a pattern in the melody"
  [pattern]
  (loop [x pattern
         f (first x)
         diffs []]
    (if (> (count x) 1)
      (recur (rest x) f (conj diffs (- f (second x))))
      diffs)))

(defn melodyPatterns 
  "Returns the number of time each pattern of length n occurs in the melody
   (this is all consecutive sequences of size n in the melody)"
  [genome n]
    (loop [notes (map #(:note %) genome)
           patterns []]
      (if (< (count notes) n)
        (vals (frequencies patterns))
        (recur (rest notes) (conj patterns (getDiffs (take n notes))))))) ;;remove getDiffs for the same notes, get diffs uses jumps of same sizes


(defn melodyPatternError 
  "Returns the error from patterns in the melody - there is a larger error if there 
   are fewer patterns"
  [genome]
  (loop [ns [1 2 4 8]
         maxReps []]
    (if (empty? ns)
      (float (reduce + maxReps))
      (recur (rest ns) (conj maxReps (/ 1 (apply max (melodyPatterns genome (first ns)))))))))


