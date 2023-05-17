# music-final

An evolutionary algorithm that generates a melody, with capacity for playback.

## Installation

Install alda and alda-player in the root directory for playback of melodies.

## How to run

First, load all functions from core.clj and error.clj. Second, load the functions from ga.clj.
Third, in demo make a call to the genetic algorithm.

The function to run the genetic algorithm is `run`. It takes the following arguments (all are required):

 - popsize: this is the initial population size
 - numgen: the maximum number of generations
 - numnotes: the number of notes in each genome
 - cases: the cases used for lexicase selection or the cases used for calculating a total error for tournament selection
 - selection: the selection method; either `:tournament` or `:lexicase`

`run` is called using the following syntax:

`(run popsize numgen numnotes cases selection)`
