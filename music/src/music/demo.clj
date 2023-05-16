(ns music.demo)
;;This includes code from all three documents for easy of running during the presentation
(require '[alda.core :refer :all])
(require '[music.error :refer :all])
(require '[music.core :refer :all])
(require '[music.ga :refer :all])


;; Generate random melody and to show improvement of our algorithm

;; ;; Run experiment with different parameters
;; ;; Cases defined is ga.clj
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



(spit "demo.txt" (run 100 100 50 cases :tournament))
(playFromFile "demo.txt")
(stop!)