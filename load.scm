(unless (assoc "code" (library-directories))
  (library-directories (cons "code" (library-directories))))

(import (trie)
        (only (euler) shuffle compose)
        (only (srfi :1) filter-map append-map))

(define src-files
  '("code/board.scm"
    "code/boggle.scm"))

(parameterize ((optimize-level 3))
  (for-all load src-files))

(define (run)
  (define board (board!))
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
