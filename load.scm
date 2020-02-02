(unless (assoc "." (library-directories))
  (library-directories (cons "." (library-directories))))

(import (gobble))

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
