(ns music.ga 
  (:require [music.core :refer :all]))

(def cases [])

(defn better
  "Return true if i1 has lower error than i2 for case 'casenum'"
  [i1 i2 caseNum]
  (< (nth (:errors i1) caseNum) (nth (:errors i2) caseNum)))

(defn tournamentSelection [pop n]
  (first (sort better (repeatedly n (rand-nth pop)))))

(defn lexicaseSelection [pop]
  (loop [survivors pop
         cases (shuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases) (empty? (rest survivors)))
      (rand-nth survivors)
      (let [minErr (apply min (map #(nth % (first cases)) (map :errors survivors)))]
        (recur (filter #(= minErr (nth (:errors %) (first cases))) survivors) (rest cases))))))

;;TODO: use some combination of crossover, selection and mutation
(defn makeChild [pop case]
  [])

(defn run [popsize numgen numnotes cases]
  (loop [curGen 0
         pop (sort better (repeatedly popsize #(getNewIndividual numnotes cases)))]
    (let [best (first pop)]
      (println "GEN: " curGen ", ERROR: " (reduce + (:errors best)))
      (if (= curGen numgen)
        best
        (recur (inc curGen) (repeatedly popsize #(makeChild pop cases)))))))