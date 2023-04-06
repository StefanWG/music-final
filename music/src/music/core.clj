(ns music.core)

(require '[alda.core :refer :all])

(alda "version")

(play!
 (part "piano")
 (note (pitch :c) (note-length 8))
 (note (pitch :d))
 (note (pitch :e))
 (note (pitch :f))
 (note (pitch :g))
 (note (pitch :a))
 (note (pitch :b))
 (octave :up)
 (note (pitch :c)))