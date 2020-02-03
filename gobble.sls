#!chezscheme
(eval-when (load compile) (optimize-level 3))
(library (gobble)
  (export gobble
          boggle

          ;; boards
          board!
          display-board
          display-ln
          roll
          *DICE*

          ;; definitions
          word?
          prefix?
          definition
          completions)
  (import (chezscheme)
          (dictionary)
          (only (euler) shuffle compose)
          (only (srfi :1) append-map filter-map))
  (include "code/board.scm")
  (include "code/boggle.scm"))
