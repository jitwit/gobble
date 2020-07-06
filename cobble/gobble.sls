#!chezscheme
(eval-when (load compile) (optimize-level 3))
(library (gobble)
  (export gobble

          ;; boards
          display-board
          display-ln
          roll
          dice-4x4
          dice-5x5
	  score-word
	  score-word-list

          ;; definitions
          word?
          prefix?
          definition
          completions)
  (import (chezscheme)
          (dictionary)
          (only (euler) shuffle compose square sort-on)
          (only (srfi :1) append-map filter-map)
	  (prefix (patricia) t:)
	  (trie))
  (include "code/board.scm")
  (include "code/boggle.scm"))
