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
  "Returns the number of time each melodic pattern of length n occurs in the melody
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
  (loop [n [1 2 4 8]
         maxReps []]
    (if (or (empty? n) (< (count genome) (first n)))
      (float (reduce + maxReps))
      (recur (rest n) (conj maxReps (/ 1 (apply max (melodyPatterns genome (first n)))))))))

(defn rhythmicPatterns
  "Returns the number of time each rythmic pattern of length n occurs in the melody
   (this is all consecutive sequences of size n in the melody)"
  [genome n]
  (loop [noteLengths (map #(:duration %) genome)
         patterns []]
    (if (< (count noteLengths) n)
      (vals (frequencies patterns))
      (recur (rest noteLengths) (conj patterns (getDiffs (take n noteLengths)))))))

(defn noteLengthConversion
  "Converts the duration representation to the mathematical length"
  [genome]
  (map (fn [x] (cond
                 (= 8 x) 0.5
                 (= 4 x) 1
                 (= 2 x) 2
                 (= 1 x) 4)) (map #(:duration %) genome)))

(defn rhythmicCoherenceError
  "Assuming 4/4 time signature. If the melody doesn't fill out a measure within two measures, it incurs an error point for each additional measure
   Not finished yet"
  [genome]
  (loop [noteLengths (noteLengthConversion genome)
         error []
         accruedLength 0]
    (let [curr (first noteLengths)]
      (cond
        (empty? noteLengths) (apply + (map #(- % 1) (filter (complement #{0 1}) (map #(int %)(conj error (quot accruedLength 4))))))
        (= 0 (mod (+ curr accruedLength) 4)) (recur (rest noteLengths) (conj error (quot accruedLength 4)) 0)
        :else (recur (rest noteLengths) error (+ curr accruedLength))))))