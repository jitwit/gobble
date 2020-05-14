#!/home/jo/.nix-profile/bin/scheme --script

(library-directories (cons "." (library-directories)))

(import (gobble)
        (only (euler) shuffle)
        (matchable))

(random-seed (time-nanosecond (current-time)))
(random-seed (random (+ (time-nanosecond (current-time))
                        (time-nanosecond (current-time))
                        (time-nanosecond (current-time)))))

(define (output-board board)
  (for-all (lambda (word)
             (display word)
             (display #\space)
             (display (definition word))
             (newline))
           (gobble board)))

;; fill directory to have n boards
(define (generate n dir)
  (define n* (length (directory-list dir)))
  (define N (- n n*))
  (format #t "there are ~a board(s), making ~a more~%" n* N)
  (do ((i 1 (1+ i)))
      ((> i N))
    (let* ((board (list->string (roll (list-head (shuffle dice-5x5) 16))))
           (board-file (string-append dir "/" board)))
      (when (file-exists? board-file)
        (error 'generate "time to make better randoms, eh?"))
      (format #t "board ~4d/~d~%" i N)
      (with-output-to-file board-file
        (lambda ()
          (output-board board))))))

(define (main)
  (match (command-line)
    ((_ "-n" n "-d" dir) (generate (string->number n) dir))
    (else (error 'gobbler "bad command line args" (command-line)))))

(main)
