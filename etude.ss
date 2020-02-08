#!/usr/bin/env scheme

(unless (assoc "~/code/gobble" (library-directories))
  (library-directories (cons "~/code/gobble" (library-directories))))

(import (gobble) (chezscheme))

(random-seed (time-nanosecond (current-time)))
(random-seed (random (time-nanosecond (current-time))))

(define N
  (if (null? (cdr (command-line)))
      1
      (with-input-from-string (cadr (command-line)) read)))

(define words
  (make-hashtable string-hash string=?))

(do ((i 0 (1+ i)))
    ((= i N)
     (vector-for-each (lambda (w.n)
                        (display (car w.n))
                        (display #\space)
                        (display (cdr w.n))
                        (newline))
                      (vector-sort (lambda (x y)
                                     (>= (cdr x) (cdr y)))
                                   (hashtable-cells words))))
  
  (for-all (lambda (word)
             (hashtable-update! words word fx1+ 0))
           (gobble (list->string (roll)))))
