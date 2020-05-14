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

(define (generate n dir)
  (do ((i 1 (1+ i)))
      ((> i n))
    (let ((board (list->string (roll (list-head (shuffle dice-5x5) 16)))))
      (format #t "board #~a~%" i)
      (with-output-to-file (string-append dir "/" board)
        (lambda ()
          (output-board board))))))

(define (main)
  (match (command-line)
    ((_ "-n" n "-d" dir) (generate (string->number n) dir))
    (else (error 'gobbler "bad command line args" (command-line)))))

(main)
