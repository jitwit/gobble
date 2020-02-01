(import (prefix (patricia) t:)
        (only (euler) shuffle)
        (only (srfi :1) filter-map append-map))

(print-gensym #f)

(define src-files
  '("code/board.scm"
    "code/trie.scm"
    "code/words.scm"
    "code/boggle.scm"))

(parameterize ((optimize-level 3))
  (for-all load src-files))

(define (run)
  (define board (board! size))
  (for-all display-ln (boggle board))
  (newline) (display-board board))

(define example-board
  '#("FXIE"
     "AMLO"
     "EWBX"
     "ASTU"))

(define demo-board
  '#("DEMO"
     "DEMO"
     "DEMO"
     "DEMO"))
