(ns music.core)

(require '[alda.core :refer :all])

(def hcb [{:pitch :b, :accidental :none, :duration 4, :octave 2},
          {:pitch :a, :accidental :flat, :duration 4, :octave 2},
          {:pitch :g, :accidental :sharp, :duration 2, :octave 2},
          {:pitch :b, :accidental :sharp, :duration 4, :octave 2},
          {:pitch :a, :accidental :flat, :duration 4, :octave 4},
          {:pitch :g, :accidental :none, :duration 2, :octave 4},
          {:pitch :g, :accidental :none, :duration 8, :octave 4},
          {:pitch :g, :accidental :none, :duration 8, :octave 3},
          {:pitch :g, :accidental :sharp, :duration 8, :octave 3},
          {:pitch :g, :accidental :none, :duration 8, :octave 3},
          {:pitch :a, :accidental :flat, :duration 8, :octave 4},
          {:pitch :a, :accidental :none, :duration 8, :octave 4},
          {:pitch :a, :accidental :none, :duration 8, :octave 4},
          {:pitch :a, :accidental :none, :duration 8, :octave 4},
          {:pitch :b, :accidental :none, :duration 4, :octave 4},
          {:pitch :a, :accidental :none, :duration 4, :octave 4},
          {:pitch :g, :accidental :none, :duration 2, :octave 4}])

;;TODO: check if note is sharp or flat 
(defn isSharp [accidental]
  "Check if note is a sharp"
  (= "sharp" (:accidental accidental)))

(defn isFlat [accidental]
  "Check if note is a flat"
  (= "flat" (:accidental accidental)))

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

;;org version without accidentals 
;; (defn toAlda [melody]
;;   "Convert melody in form of [{:pitch :duration : octave}] to alda"
;;   (loop [seq [(part "piano")
;;               (octave (:octave (first melody)))]
;;          curNote (first melody)
;;          notes (rest melody)
;;          prevOctave (:octave (first melody))]
;;     (let [octaveChange (getOctaveChange prevOctave (:octave curNote)) ;; get necessary octave changes
;;           newNote (if (isRest curNote) ;; create new note
;;                     (pause (note-length (:duration curNote))) ;; pause note
;;                     (note (pitch (:pitch curNote)) (note-length (:duration curNote)))) ;; normal note
;;           newSeq (conj seq octaveChange newNote)] ;; append octave changes and new note to current sequence
;;       (if (= (count notes) 0)
;;         newSeq
;;         (recur newSeq (first notes) (rest notes) (:octave curNote))))))


;;version with accidentals
(defn toAlda [melody]
  "Convert melody in form of [{:pitch :duration : octave}] to alda"
  (loop [seq [(part "piano")
              (octave (:octave (first melody)))]
         curNote (first melody)
         notes (rest melody)
         prevOctave (:octave (first melody))]
    (let [octaveChange (getOctaveChange prevOctave (:octave curNote)) ;; get necessary octave changes
          newNote (if-not (isRest curNote) ;; create new note
                    (if (isSharp curNote)
                      (note (pitch (:pitch curNote) (:accidental curNote)) (note-length (:duration curNote)))
                      (if (isFlat curNote)
                        (note (pitch (:pitch curNote) (:accidental curNote)) (note-length (:duration curNote)))
                        (note (pitch (:pitch curNote)) (note-length (:duration curNote)))))
                    (pause (note-length (:duration curNote)))) ;; pause note
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

(defn getRandomAccidental []
  (rand-nth [:sharp :flat :none])) ;;TODO: What probabilities?

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
            octave (getRandomOctave)
            accidental (getRandomAccidental)]
        (recur (- notesLeft (/ 4 noteSize)) (conj melody 
                                                  {:pitch pitch 
                                                   :duration noteSize 
                                                   :octave octave
                                                   :accidental accidental}))))))

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

(defn getlength "Returns binomial sum of length 10 w/ prob 0.5"
  [] (reduce + (random-sample 0.5 [1 1 1 1 1 1 1 1 1 1])))