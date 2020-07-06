(library-directories (cons "." (library-directories)))
(optimize-level 3)
(import (gobble)
        (euler))

(define (gobble1 board)
  (time (begin (gobble board) (void))))

(define (do-gobble board N)
  (do ((i 0 (fx1+ i)))
      ((fx= i N))
    (gobble1 board)))

(define example1 "SKDSMAHLETROEUSP")
(define example2 "ELIHYWOTXSERINAT")

(define (run-bench)
  (define out "../report/bench.txt")
  (when (file-exists? out)
    (delete-file out))
  (transcript-on out)
  (do-gobble example1 1000)
  (transcript-off))
