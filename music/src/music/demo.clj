(ns music.demo)
(require '[alda.core :refer :all])
(require '[music.error :refer :all])
(require '[music.core :refer :all])
(require '[music.ga :refer :all])
(require '[music.nn :refer :all])


(def cases [restError rhythmicCoherenceError
            melodyPatternError distanceError
            variationError octaveChangeError
            diversityError neuralNetError])
;; Generate random melody and to show improvement of our algorithm

;; ;; Run experiment with different parameters
;; ;; Cases defined is ga.clj

(getNN "nn.txt")
(for [popsize [50 100 200]
      numgen [50 100 200]
      numnotes [20 30]
      selection [:tournament :lexicase]]
  (loop [i 0]
    (let [fileName (str (name selection)"/" popsize "_" numgen "_" numnotes "_" i ".txt")]
      (println fileName)
      (if (< i 1)
        (do
          (spit fileName (run popsize numgen numnotes cases selection))
          (recur (inc i)))))))


;; Play melodies that sound good - what parameters
(playFromFile "file_200_200_30_2.txt") ;;Sounds good
(playFromFile "file_200_200_50_1.txt") ;;Converges to single note
(playFromFile "file_200_200_50_0.txt") ;;also sounds good

(getNN "nn.txt")
(run 100 100 20 cases :lexicase)

(spit "tournament.txt" (run 100 100 20 cases :tournament))
(spit "lexicase.txt" (run 100 100 20 cases :lexicase))

(playFromFile "tournament.txt")
(playFromFile "lexicase.txt")

(stop!)

(defn formatGenome [genome]
  (let [mel (vec (map #(:note % ) genome))]
    (into mel (repeat (- 32 (count mel)) 0))))

(neuralNetError (getNewGenome 20))