(ns music.ga)
(require '[music.core :refer :all])
(require '[music.error :refer :all])


(def cases [restError rhythmicCoherenceError melodyPatternError])

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
  p1)

(defn binomsample [n r]
  (reduce + (random-sample r (vec (repeat n 1)))))

(defn mutate_note [note]
  (let [diff (- (binomsample 10 0.5) 5)
        result (+ note diff)]
    (if (> result -1)
      (if (< result 128) result 127) 0)))

(defn mutate [genome mutation-rate]
  (map (fn [note]
         (if (< (rand) mutation-rate)
           (assoc note :duration (getRandomNoteSize))
           note))
       (map (fn [note]
              (if (< (rand) mutation-rate)
                (assoc note :note (mutate_note (:note note)))
                note))
            genome)))

;;TODO: use some combination of crossover, selection and mutation
(defn makeChild [pop cases]
  (let [parent1 (:genome (select pop))
        parent2 (:genome (select pop))
        newGenome (mutate (crossover parent1 parent2) 0.01)]
    {:genome newGenome
     :errors (errors newGenome cases)}))

(defn run [popsize numgen numnotes cases]
  (loop [curGen 0
         pop (sort better (repeatedly popsize #(getNewIndividual numnotes cases)))]
    (let [best (first pop)]
      (println "GEN: " curGen ", ERROR: " (reduce + (:errors best)))
      (if (= curGen numgen)
        best
        (recur (inc curGen) (repeatedly popsize #(makeChild pop cases)))))))

(run 100 100 100 cases)