#!/usr/bin/env scheme --script

(library-directories (cons "." (library-directories)))

(import (gobble)
        (only (euler) shuffle)
        (matchable))

(random-seed (time-nanosecond (current-time)))
(random-seed (random (+ (time-nanosecond (current-time))
                        (time-nanosecond (current-time))
                        (time-nanosecond (current-time)))))

(define (dump-solution solution)
  (for-all (lambda (word.def)
             (display (car word.def))
             (display #\space)
             (display (cdr word.def))
             (newline))
           solution))

(define (interesting-board? solution)
  (let ((longest-word (if (null? solution)
                          0
                          (string-length (caar (last-pair solution))))))
    ;; still accept "tough" boards but mostly make sure there are words to find.
    (or (<= 8 longest-word)
        (cond
         ((<= 7 longest-word) (< 15/20 (random 1.0)))
         ((<= 6 longest-word) (< 1/2   (random 1.0)))
         ((<= 5 longest-word) (< 1/2   (random 1.0)))
         (else #f)))))

;; fill directory to have n boards
(define (generate n dir)
  (define n* (length (directory-list dir)))
  (define N (- n n*))
  (format #t "there are ~a board(s), making ~a more~%" n* N)
  (let make ((i 1))
    (unless (> i N)
      (let* ((board (roll (list-head (shuffle dice-5x5) 16)))
             (board-file (string-append dir "/" board))
             (solution (gobble board)))
        (cond
         ((file-exists? board-file)
          (error 'generate "time to make better randoms, eh?"))
         ((interesting-board? solution)
          (format #t "saving    ~s ~4d/~d~%" board i N)
          (with-output-to-file board-file
            (lambda ()
              (dump-solution solution)))
          (make (1+ i)))
         (else
          (format #t "rejecting ~s~%" board)
          (make i)))))))

(define (main)
  (match (command-line)
    ((_ "-n" n "-d" dir) (generate (string->number n) dir))
    (else (error 'gobbler "bad command line args" (command-line)))))

(main)

