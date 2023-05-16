(ns music.ga)
(require '[music.core :refer :all])
(require '[music.error :refer :all])


(def cases [restError rhythmicCoherenceError 
            melodyPatternError distanceError 
            variationError octaveChangeError
            diversityError])

(defn better
  [i1 i2]
  (< (reduce + (:errors i1)) (reduce + (:errors i2))))

(defn tournamentSelection [pop n]
  (first (sort better (repeatedly n (rand-nth pop)))))

(defn lexicaseSelection [pop]
  (loop [survivors pop
         cases (shuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases) (empty? (rest survivors)))
      (rand-nth survivors)
      (let [minErr (apply min (map #(nth % (first cases)) (map :errors survivors)))]
        (recur (filter #(= minErr (nth (:errors %) (first cases))) survivors) (rest cases))))))

(defn select [pop]
  (lexicaseSelection pop))

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

(defn makeChild [pop cases]
  (let [parent1 (:genome (select pop))
        parent2 (:genome (select pop))
        newGenome (mutatePattern (vec (mutate (crossover parent1 parent2) 0.01)) 0.02)]
    {:genome newGenome
     :errors (errors newGenome cases)}))

(defn run [popsize numgen numnotes cases]
  (loop [curGen 0
         pop (sort better (repeatedly popsize #(getNewIndividual numnotes cases)))]
    (let [best (first (sort better pop))]
      (println "GEN: " curGen ", ERROR: " (reduce + (:errors best)) ", " (nth (:errors best) 0))
      (if (= curGen numgen)
        best
        (recur (inc curGen) (conj (repeatedly (- popsize 1) #(makeChild pop cases)) best))))))





(for [popsize [50 100 200]
      numgen [50 100 200]
      numnotes [20 30 50]]
  (loop [i 0]
    (let [fileName (str "file_" popsize "_" numgen "_" numnotes "_" i ".txt")]
                 (println fileName)

      (if (< i 3)
         (do
           (spit fileName (run popsize numgen numnotes cases))
           (recur (inc i)))
        ))))
