(ns music.core)

(require '[alda.core :refer :all])

(def hcb [{:pitch :b, :duration 4, :octave 2},
          {:pitch :a, :duration 4, :octave 2},
          {:pitch :g, :duration 2, :octave 2},
          {:pitch :b, :duration 4, :octave 2},
          {:pitch :a, :duration 4, :octave 4},
          {:pitch :g, :duration 2, :octave 4},
          {:pitch :g, :duration 8, :octave 4},
          {:pitch :g, :duration 8, :octave 3},
          {:pitch :g, :duration 8, :octave 3},
          {:pitch :g, :duration 8, :octave 3},
          {:pitch :a, :duration 8, :octave 4},
          {:pitch :a, :duration 8, :octave 4},
          {:pitch :a, :duration 8, :octave 4},
          {:pitch :a, :duration 8, :octave 4},
          {:pitch :b, :duration 4, :octave 4},
          {:pitch :a, :duration 4, :octave 4},
          {:pitch :g, :duration 2, :octave 4}])


(defn isRest [note]
  "Check if note is a rest note"
  (= "rest" (:pitch note)))

(defn getOctaveChange [prevOctave nextOctave]
  "Get alda code for octave change between previous note and current note"
  (let [n (- prevOctave nextOctave)]
    (cond
      (< n 0) (repeat (abs n) (octave :up))
      (> n 0) (repeat n (octave :down))
      :else [])))

(defn toAlda [melody]
  "Convert melody in form of [{:pitch :duration : octave}] to alda"
  (loop [seq [(part "piano")
              (octave (:octave (first melody)))]
         curNote (first melody)
         notes (rest melody)
         prevOctave (:octave (first melody))]
    (let [octaveChange (getOctaveChange prevOctave (:octave curNote)) ;; get necessary octave changes
          newNote (if (isRest curNote) ;; create new note
                    (pause (note-length (:duration curNote))) ;; pause note
                    (note (pitch (:pitch curNote)) (note-length (:duration curNote)))) ;; normal note
          newSeq (conj seq octaveChange newNote)] ;; append octave changes and new note to current sequence
      (if (= (count notes) 0)
        newSeq
        (recur newSeq (first notes) (rest notes) (:octave curNote))))))

(play! (toAlda hcb))


