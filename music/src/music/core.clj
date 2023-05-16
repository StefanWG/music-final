(ns music.core)

(require '[alda.core :refer :all])

(defn isRest
  "Check if note is a rest note"
  [note]
  (= (:note note) -1))

(defn getOctaveChange
  "Get alda code for octave change between previous note and current note"
  [prevOctave nextOctave]
  (let [n (- prevOctave nextOctave)]
    (cond
      (< n 0) (repeat (abs n) (octave :up))
      (> n 0) (repeat n (octave :down))
      :else [])))

(defn getNoteData
  "Using note table in Appendix 1.3 on http://www.music.mcgill.ca/~ich/classes/mumt306/StandardMIDIfileformat.html"
  [note]
  (let [octave (dec (Math/floor (/ note 12)))
        pitch (nth [:c :c :d :d :e :f :f :g :g :a :a :b] (mod note 12))
        accidental (nth [:none :sharp :none :sharp :none :none :sharp :none :sharp :none :sharp :none] (mod note 12))]
    [(int octave) pitch accidental]))

(defn toAlda
  "Convert melody in form of [{:note :duration}] to alda"
  [melody]
  (loop [curNote (first melody)
         notes (rest melody)
         prevOctave (max (first (getNoteData (:note curNote))) 0)
         seq [(part "piano")
              (tempo 160)
              (octave prevOctave)]]
    (let [[curOctave curPitch curAccidental] (getNoteData (:note curNote))
          octaveChange (getOctaveChange prevOctave curOctave) ;; get necessary octave changes
          newNote (if (isRest curNote)
                    (pause (note-length (:duration curNote))) ;; rest
                    (if (= curAccidental :none)
                      (note (pitch curPitch) (note-length (:duration curNote)))
                      (note (pitch curPitch curAccidental) (note-length (:duration curNote)))))] ;; normal note
      (if (= (count notes) 0)
        (conj seq octaveChange newNote)
        (recur (first notes) (rest notes) curOctave (conj seq octaveChange newNote))))))


(defn play
  "Play a melody"
  [melody]
  (play! (toAlda melody)))

(defn playFromFile [filepath]
  (play (:genome (read-string (slurp filepath)))))

(defn getlength "Returns binomial sum of length n w/ prob 0.5"
  [n] (reduce + (random-sample 0.5 (vec (repeat n 1)))))

(defn binomsample "Returns binomial sum of length n w/ prob r"
  [n r]
  (reduce + (random-sample r (vec (repeat n 1)))))

(defn pauseTime
  "Prevents overlap of multiple files being played at the same time"
  [x]
  (Thread/sleep (* x 1000)))

(defn playAllFiles
  "Plays all the generated files"
  []
  (for [popsize [50 100 200]
        numgen [50 100 200]
        numnotes [20 30 50]]
    (loop [i 0]
      (let [fileName (str "file_" popsize "_" numgen "_" numnotes "_" i ".txt")
            secondsToWait (cond (= numnotes 20) 14
                                (= numnotes 30) 17
                                (= numnotes 50) 22)]
        (println fileName)
        (if (< i 3)
          (do
            (playFromFile fileName)
            (pauseTime secondsToWait)
            (recur (inc i))))))))