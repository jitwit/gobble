#!chezscheme

(eval-when (load compile) (optimize-level 3))

(library (gobble)
  (export gobble
          boggle

          board!
          display-board
          display-ln)
  (import (chezscheme)
          (dictionary)
          (only (euler) shuffle compose)
          (only (srfi :1) filter-map append-map))

  (include "code/board.scm")
  (include "code/boggle.scm")

  (define gobble
    (compose boggle string->board))
  
  )
