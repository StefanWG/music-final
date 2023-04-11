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

(defn play [melody]
  "Play a melody"
  (play! (toAlda melody)))

(defn getRandomNoteSize []
  (let [n (rand)]
    (cond
      (< n 0.2) 8 ;;eigth note w prob 20%
      (< n 0.6) 4 ;;quarter note w prob 40%
      (< n 0.8) 2 ;;half note w prob 20%
      :else 1))) ;;full note w prob 20%

(defn getRandomPitch []
  (rand-nth [:a :b :c :d :e :f :g]))

(defn getRandomOctave[] 
  (+ 1 (rand-int 8)))

;;NOTE: The individual will have a length at least as large as numNotes
;; but it can be up to 3.5 beats larger
(defn getNewGenome [numNotes] ;;In terms of quarters notes
  (loop [notesLeft numNotes
         melody []]
    (if (<= notesLeft 0)
      melody
      (let [noteSize (getRandomNoteSize)
            pitch (getRandomPitch)
            octave (getRandomOctave)]
        (recur (- notesLeft (/ 4 noteSize)) (conj melody {:pitch pitch :duration noteSize :octave octave}))))))

;;TODO How do we do this?
(defn errors
  "Calculate errors for a given genome"
  [genome cases]
  []) 

(defn getNewIndividual [numNotes cases]
  (let [genome (getNewGenome numNotes)]
    {:genome genome
     :errors (errors genome cases)}))

(getNewIndividual 16 [])