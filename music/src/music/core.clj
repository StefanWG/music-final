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

;;testing

;;TODO: check if note is sharp or flat 
(defn isSharp [accidental]
  "Check if note is a sharp"
  (= :sharp (:accidental accidental)))

(defn isFlat [accidental]
  "Check if note is a flat"
  (= :flat (:accidental accidental)))

(defn isRest [note]
  "Check if note is a rest note"
  (= note -1))

(defn getOctaveChange [prevOctave nextOctave]
  "Get alda code for octave change between previous note and current note"
  (let [n (- prevOctave nextOctave)]
    (cond
      (< n 0) (repeat (abs n) (octave :up))
      (> n 0) (repeat n (octave :down))
      :else [])))

(defn getNoteData [note]
  "Using note table in Appendix 1.3 on http://www.music.mcgill.ca/~ich/classes/mumt306/StandardMIDIfileformat.html"
  (let [octave (dec (Math/floor (/ note 12)))
        pitch (nth [:c :c :d :d :e :f :f :g :g :a :a :b] (mod note 12))
        accidental (nth [:none :sharp :none :sharp :none :none :sharp :none :sharp :none :sharp :none] (mod note 12))]
    [(int octave) pitch accidental]))


(defn toAlda [melody]
  "Convert melody in form of [{:note :duration}] to alda"
  (loop [curNote (first melody)
         notes (rest melody)
         prevOctave (first (getNoteData (:note curNote)))
         seq [(part "piano")
              (octave prevOctave)]]
    (let [[curOctave curPitch curAccidental] (getNoteData (:note curNote))
          octaveChange (getOctaveChange prevOctave curOctave) ;; get necessary octave changes
          newNote (if-not (isRest curNote) ;; create new note
                    (if (= curAccidental :none)
                      (note (pitch curPitch) (note-length (:duration curNote)))
                      (note (pitch curPitch curAccidental) (note-length (:duration curNote))))
                    (pause (note-length (:duration curNote))))] ;; normal note
      (if (= (count notes) 0)
        (conj seq octaveChange newNote)
        (recur (first notes) (rest notes) curOctave (conj seq octaveChange newNote))))))


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

(defn getRandomNote []
  (- (rand-int 129) 1))

;;NOTE: The individual will have a length at least as large as numNotes
;; but it can be up to 3.5 beats larger
(defn getNewGenome [numNotes] ;;In terms of quarters notes
  (loop [notesLeft numNotes
         melody []]
    (if (<= notesLeft 0)
      melody
      (let [noteSize (getRandomNoteSize)
            note (getRandomNote)]
        (recur (- notesLeft (/ 4 noteSize)) (conj melody
                                                  {:note note
                                                   :duration noteSize}))))))

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

(defn getlength "Returns binomial sum of length n w/ prob 0.5"
  [n] (reduce + (random-sample 0.5 (vec (repeat n 1)))))

(getlength 100)