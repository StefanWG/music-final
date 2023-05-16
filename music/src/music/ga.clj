(ns music.ga)
(require '[music.error :refer :all])
(require '[music.core :refer :all])


(defn getRandomNote []
  (- (rand-int 129) 1))

(defn getRandomNoteSize []
  ;; 4)
  (let [n (rand)]
    (cond
      (< n 0.25) 8 ;;eigth note w prob 20%
      (< n 0.7) 4 ;;quarter note w prob 40%
      (< n 0.9) 2 ;;half note w prob 20%
      :else 1))) ;;full note w prob 20%

(defn getNewGenome [numNotes]
  (loop [notesLeft numNotes
         melody []]
    (if (< notesLeft 1)
      melody
      (recur (dec notesLeft) (conj melody {:note (getRandomNote)
                                           :duration (getRandomNoteSize)})))))

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

(defn mutatePattern "Given a melody, selects a four note pattern at random and duplicates it,
                    replacing the four notes that succeed it.
                    If there are not at least 5 notes nothing happens"
  [patternGenome dupe-rate]
  (let [length (count patternGenome)
        ;Pattern must start 8 before the end, or else at least at 0
        index (rand-int (max (- length 7) 0))
        ;We draw the pattern from the index which we know is valid to include the next three notes, or at least what remains until the end
        pattern (subvec patternGenome index (min length (+ index 4)))
        ;If the whole patten cannot be fit repeatLength gives the remaining notes. Else this is 4
        repeatLength (min 4 (- length (count pattern)))
        repeated (subvec pattern repeatLength)]
    (if (< (rand) dupe-rate)
      (if (< length 5) patternGenome
        ;as long as at least one note can be repeated
          (loop [n (+ index 4)
                 final patternGenome]
          ;if current n has passed index + 4 + repeatLength - 1, ie past the stopping point for the repeated pattern
          ;
            (if (>= n (+ (+ index 4) repeatLength)) final
              ;otherwise we replace the appropriate element
              ;this is n minus the starting index of the pattern + 4
                (recur (inc n) (assoc final n (get pattern (- n (+ index 4)))))))) patternGenome)))

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
        newGenome (mutatePattern (vec (mutate (crossover parent1 parent2) 0.01)) 0.02)]
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


(def cases [restError rhythmicCoherenceError
            melodyPatternError distanceError
            variationError octaveChangeError
            diversityError])