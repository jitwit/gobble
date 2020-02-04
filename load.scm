(unless (assoc "." (library-directories))
  (library-directories (cons "." (library-directories))))

(import (dictionary)
        (gobble))

(define (run)
  (define board (board!))
  (for-all display-ln (boggle board))
  (newline) (display-board board))

(define (dump-collins)
  (define out-file "clns.txt")
  (delete-file out-file)
  (with-output-to-file out-file
    (lambda ()
      (define out (current-output-port))
      (define (put-def def)
        (display "(" out)
        (write (car def) out)
        (display ", " out)
        (write (cdr def) out)
        (display ")\n" out))
      (for-all put-def (get-collins)))))

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
