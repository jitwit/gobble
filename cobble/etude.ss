(library-directories (cons "." (library-directories)))
(optimize-level 3)
(import (gobble)
        (euler))

(define (frequency-etude N)
  (define V (make-fxvector 20 0))
  (random-seed 2) ; "reproducible" idk
  (let walk ((N N))
    (if (zero? N)
        (map list (iota 20) (fxvector->list V))
        (let* ((board (list->string (roll (list-head (shuffle dice-5x5) 16))))
               (solution (gobble board))
               (n (if (null? solution)
                      0
		      (string-length (caar (last-pair solution))))))
	  (fxvector-inc! V n)
          (walk (1- N))))))

(define (score-etude N)
  (define T (make-eqv-hashtable))
  (define best)
  (random-seed 2) ; "reproducible" idk
  (do ((i 0 (fx1+ i)))
      ((fx= i N) (hashtable-cells T))
    (hashtable-update! T
		       (score-word-list
			(map car
			     (gobble
			      (list->string
			       (roll (list-head (shuffle dice-5x5) 16))))))
		       fx1+
		       0)))

(define (output-results results N)
  (for-each (lambda (n.f)
              (format #t "~2a ~,2f~%" (car n.f) (/ (cadr n.f) N)))
            results))

(define (report-frequency N)
  (define out-file "../report/freq.txt")
  (define results (frequency-etude N))
  (when (file-exists? out-file)
    (delete-file out-file))
  (output-results results N)
  (with-output-to-file out-file
    (lambda ()
      (for-each (lambda (n.f)
                  (apply format #t "~a ~a~%" n.f))
                results))))

(define (report-scores N)
  (define out-file "../report/scores.txt")
  (define results (scores-etude N))
  (when (file-exists? out-file)
    (delete-file out-file))
  (output-results results N)
  (with-output-to-file out-file
    (lambda ()
      (for-each (lambda (n.f)
                  (apply format #t "~a ~a~%" n.f))
                results))))
