(unless (assoc "~/code/gobble" (library-directories))
  (library-directories (cons "~/code/gobble" (library-directories))))

(import (gobble) (only (euler) shuffle))

(random-seed (time-nanosecond (current-time)))
(random-seed (random (time-nanosecond (current-time))))

(let* ((args (cdr (command-line)))
       (dice (cond
              ((null? args)
               (error 'gobbler
                      "expecting command-line arguments \"5x5\" or \"4x4\""))
              ((string=? (car args) "5x5") dice-5x5)
              ((string=? (car args) "4x4") (list-head (shuffle dice-5x5) 16))
              (else
               (error 'gobbler
                      "expecting command-line arguments \"5x5\" or \"4x4\" but got"
                      args))))
       (board (list->string (roll dice))))
  (display-ln board)
  (for-all (lambda (word)
             (display word)
             (display #\space)
             (display (definition word))
             (newline))
           (gobble board)))
