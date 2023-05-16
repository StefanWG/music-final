(ns music.demo
  (:require [clojure.java.shell :as shell]))
;;This includes code from all three documes for easy of running during the presentation
(require '[alda.core :refer :all])


(defn isRest
  "Check if note is a rest note"
  [note]
  (= (:note note) -1))

(defn getOctaveChange
  "Get alda code for octave change between previous note and current note"
  [prevOctave nextOctave]
  (let [n (- prevOctave nextOctave)]
    (cond
      (< n 0) (repeat (abs n) (octave :up))
      (> n 0) (repeat n (octave :down))
      :else [])))

(defn getNoteData
  "Using note table in Appendix 1.3 on http://www.music.mcgill.ca/~ich/classes/mumt306/StandardMIDIfileformat.html"
  [note]
  (let [octave (dec (Math/floor (/ note 12)))
        pitch (nth [:c :c :d :d :e :f :f :g :g :a :a :b] (mod note 12))
        accidental (nth [:none :sharp :none :sharp :none :none :sharp :none :sharp :none :sharp :none] (mod note 12))]
    [(int octave) pitch accidental]))


(defn toAlda
  "Convert melody in form of [{:note :duration}] to alda"
  [melody]
  (loop [curNote (first melody)
         notes (rest melody)
         prevOctave (max (first (getNoteData (:note curNote))) 0)
         seq [(part "piano")
              (tempo 160)
              (octave prevOctave)]]
    (let [[curOctave curPitch curAccidental] (getNoteData (:note curNote))
          octaveChange (getOctaveChange prevOctave curOctave) ;; get necessary octave changes
          newNote (if (isRest curNote)
                    (pause (note-length (:duration curNote))) ;; rest
                    (if (= curAccidental :none)
                      (note (pitch curPitch) (note-length (:duration curNote)))
                      (note (pitch curPitch curAccidental) (note-length (:duration curNote)))))] ;; normal note
      (if (= (count notes) 0)
        (conj seq octaveChange newNote)
        (recur (first notes) (rest notes) curOctave (conj seq octaveChange newNote))))))


(defn play
  "Play a melody"
  [melody]
  (play! (toAlda melody)))

(defn getRandomNoteSize []
  ;; 4)
  (let [n (rand)]
    (cond
      (< n 0.25) 8 ;;eigth note w prob 20%
      (< n 0.7) 4 ;;quarter note w prob 40%
      (< n 0.9) 2 ;;half note w prob 20%
      :else 1))) ;;full note w prob 20%

(defn getRandomNote []
  (- (rand-int 129) 1))

(defn getNewGenome [numNotes]
  (loop [notesLeft numNotes
         melody []]
    (if (< notesLeft 1)
      melody
      (recur (dec notesLeft) (conj melody {:note (getRandomNote)
                                           :duration (getRandomNoteSize)})))))

(defn totalError
  "Normalizes all cases of an individual's error"
  [maxErrors errors]
  (loop [maxErrLeft maxErrors
         errorsLeft errors
         normalized []]
    (if (empty? errorsLeft)
      (reduce + normalized)
      (let [max-val (first maxErrors)]
        (recur (rest maxErrLeft) (rest errorsLeft) (conj normalized (/ (first errorsLeft) max-val)))))))

(defn findMax
  "Find maximum error for each case"
  [pop c]
  (loop [cases (range (count c))
         maxErrors []]
    (if (empty? cases)
      maxErrors
      (let [maxErr (apply max (map #(nth % (first cases)) (map :errors pop)))]
        (recur (rest cases) (conj maxErrors maxErr))))))

(defn errors
  "Calculate errors for a given genome"
  [genome cases]
  (loop [casesLeft cases
         errors []]
    (if (empty? casesLeft)
      errors
      (recur (rest casesLeft) (conj errors ((first casesLeft) genome))))))

(defn getNewIndividual [numNotes cases]
  (let [genome (getNewGenome numNotes)]
    {:genome genome
     :errors (errors genome cases)}))

(defn getNewPopulation
  [popsize numnotes cases]
  (let [pop (repeatedly popsize #(getNewIndividual numnotes cases))
        maxErrs (findMax pop cases)]
    (map #(assoc % :totalError (totalError maxErrs (:errors %))) pop)))

(defn playFromFile [filepath]
  (play (:genome (read-string (slurp filepath)))))


(defn octaveChangeError
  "Returns the error due to large difference between consecutive notes"
  [genome]
  (let [len (count genome)]
    (loop [numChanges 0
           notes (map #(:note %) genome)]
      (if (= (count notes) 1)
        (max 0 (- (/ numChanges len) 0.02))
        (recur (+ numChanges (Math/floorDiv (abs (- (first notes) (second notes))) 12)) (rest notes))))))

(defn restError
  "Returns the number of rests in the melody (more rests means more error)"
  [genome]
  ;;Could be easily modified to include the length of rests with a 
  ;;larger punishment for longer rests
  (let [numRests (count (filter #(= (:note %) -1) genome))
        error (- (/ numRests (count genome)) 0.25)]
    (max 0 error)))

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
      (vec (vals (frequencies patterns)))
    ;;   (vec (vals (frequencies patterns)))
      (recur (rest notes) (conj patterns (getDiffs (take n notes))))))) ;;remove getDiffs for the same notes, get diffs uses jumps of same sizes


(defn avg [coll]
  (/ (reduce + coll) (count coll)))

(defn melodyPatternError
  "Returns the error from patterns in the melody - there is a larger error if there 
   are fewer patterns"
  [genome]
  (loop [n [2 4 8]
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
        (empty? noteLengths) (apply + (map #(- % 1) (filter (complement #{0 1}) (map #(int %) (conj error (quot accruedLength 4))))))
        (= 0 (mod (+ curr accruedLength) 4)) (recur (rest noteLengths) (conj error (quot accruedLength 4)) 0)
        :else (recur (rest noteLengths) error (+ curr accruedLength))))))

(defn averageNote
  "Calculates the average note of the melody (add all numerical rep of notes / number of notes in melody)"
  [genome]
  (/ (reduce + (filter #(not= (:note %) -1) (for [x genome] (get x :note)))) (count (filter #(not= (:note %) -1) genome))))

(defn distanceError
  "Punishes total distance from the average note"
  [genome]
  (reduce + (map #(abs (- % (averageNote genome))) (filter #(not= % -1) (for [x genome] (get x :note))))))

(defn variationError
  "Punishes a small variation"
  [genome]
  (/ 1 (- (apply max (filter #(not= (:note %) -1) (for [x genome] (get x :note)))) (apply min (filter #(not= (:note %) -1) (for [x genome] (get x :note)))))))

(defn distinctNotes
  "Return hashmap with key being a note, and the value as number of times note appears"
  [genome]
  (loop [distinctNotes {}
         remainingNotes (map #(:note %) genome)]
    (if (empty? remainingNotes)
      distinctNotes
      (let [currNote (first remainingNotes)]
        (if (contains? distinctNotes currNote)
          (recur (assoc distinctNotes currNote (inc (get distinctNotes currNote))) (rest remainingNotes))
          (recur (assoc distinctNotes currNote 1) (rest remainingNotes)))))))

(defn diversityError
  "If more than 2/5 of the melody is composed of the same note, punish the genome"
  [genome]
  (let [distinctNoteCounts (vals (distinctNotes genome))
        totalNotes (count genome)]
    (count (filter #(>= (/ % totalNotes) (/ 2 5)) distinctNoteCounts))))

(defn better
  [i1 i2]
  (< (reduce + (:errors i1)) (reduce + (:errors i2))))

(defn betterTotal [i1 i2]
  (< (:totalError i1) (:totalError i2)))

(defn fittest
  "Returns the fittest of the given individuals."
  [pop]
  (reduce (fn [i1 i2]
            (if (< (:totalError i1) (:totalError i2))
              i1
              i2))
          pop))

(defn tournamentSelection
  "Returns an individual selected from population using a tournament."
  [population]
  (fittest (repeatedly 2 #(rand-nth population))))

(defn lexicaseSelection [pop]
  (loop [survivors pop
         cases (shuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases) (empty? (rest survivors)))
      (rand-nth survivors)
      (let [minErr (apply min (map #(nth % (first cases)) (map :errors survivors)))]
        (recur (filter #(= minErr (nth (:errors %) (first cases))) survivors) (rest cases))))))

(defn select [pop selection]
  (cond
    (= selection :lexicase) (lexicaseSelection pop)
    (= selection :tournament) (tournamentSelection pop)))
  
(defn crossover [p1 p2]
  (loop [newGenome []
         i 0]
    (if (= i (min (count p1) (count p2)))
      newGenome
      (let [newNote (if (< (rand) 0.5)
                      (nth p1 i)
                      (nth p2 i))]
        (recur (conj newGenome newNote) (inc i))))))

(defn getlength "Returns binomial sum of length n w/ prob 0.5"
  [n] (reduce + (random-sample 0.5 (vec (repeat n 1)))))

(defn binomsample "Returns binomial sum of length n w/ prob r"
  [n r]
  (reduce + (random-sample r (vec (repeat n 1)))))

(defn mutate_note "Takes in an integer note then mutates it according to a binomial distribution with mean zero and max absolute difference of 5.
                   Also adheres to the floor and ceiling of the notes table. If a mutation will go past this it will just round down."
  [note]
  (let [restProb 0.1
        diff (- (binomsample 10 0.5) 5)
        result (+ note diff)]
    (if (= -1 note)
      (if (< (rand) restProb)  ;; Rest
        result
        -1)
      (if (< (rand) restProb)
        -1
        (if (> result -1) ;; Not a rest
          (if (< result 128) result 127) 0)))))

(defn mutate "Takes in a melody genome and mutation rate between 0 and 1 inclusive. For each note, with probability mutation rate,
              there is a chance that the note will be mutated with mutate_note.
              Also, a separate pass with the same probability mutation rate has the chance to mutate the note length with getRandomNoteSize."
  [genome mutation-rate]
  (map (fn [note]
         (if (< (rand) mutation-rate)
           (assoc note :duration (getRandomNoteSize))
           note))
       (map (fn [note]
              (if (< (rand) mutation-rate)
                (assoc note :note (mutate_note (get note :note)))
                note))
            genome)))

(defn makeChild [pop cases selection]
  (let [parent1 (:genome (select pop selection))
        parent2 (:genome (select pop selection))
        newGenome (mutate (crossover parent1 parent2) 0.01)]
    {:genome newGenome
     :errors (errors newGenome cases)}))

(defn run [popsize numgen numnotes cases selection]
  (loop [curGen 0
         pop (getNewPopulation popsize  numnotes cases)]
    (let [best (first (sort betterTotal pop))]
      (println "GEN: " curGen ", ERROR: " (:totalError best))
      (if (= curGen numgen)
        best
        (let [newPop (conj (repeatedly (- popsize 1) #(makeChild pop cases selection)) best)
              maxErrs (findMax pop cases)]
          (recur (inc curGen) 
                 (map #(assoc % :totalError (totalError maxErrs (:errors %))) newPop)))))))

(defn readBachDataset []
  (map read-string (clojure.string/split-lines (slurp "melodies.txt"))))

(def cases [restError rhythmicCoherenceError
            melodyPatternError distanceError
            variationError octaveChangeError
            diversityError])

;; Generate random melody and to show improvement of our algorithm

;; ;; Run experiment with different parameters

(for [popsize [50 100 200]
      numgen [50 100 200]
      numnotes [20 30 50]
      selection [:tournament :lexicase]]
  (loop [i 0]
    (let [fileName (str (name selection)"_" popsize "_" numgen "_" numnotes "_" i ".txt")]
      (println fileName)
      (if (< i 1)
        (do
          (spit fileName (run popsize numgen numnotes cases selection))
          (recur (inc i)))))))

;; Play melodies that sound good - what parameters
(playFromFile "file_200_200_30_2.txt") ;;Sounds good
(playFromFile "file_200_200_50_1.txt") ;;Converges to single note
(playFromFile "file_200_200_50_0.txt") ;;also sounds good

(defn pauseTime 
  "Prevents overlap of multiple files being played at the same time"
  [x]
  (Thread/sleep (* x 1000)))

(defn playAllFiles
  "Plays all the generated files"
  []
  (for [popsize [50 100 200]
        numgen [50 100 200]
        numnotes [20 30 50]]
    (loop [i 0]
      (let [fileName (str "file_" popsize "_" numgen "_" numnotes "_" i ".txt")
            secondsToWait (cond (= numnotes 20) 14
                                (= numnotes 30) 17
                                (= numnotes 50) 22)]
        (println fileName)
        (if (< i 3)
          (do
            (playFromFile fileName)
            (pauseTime secondsToWait)
            (recur (inc i))))))))

(spit "demo.txt" (run 100 100 20 cases :lexicase))
(playFromFile "demo.txt")

