#!/usr/bin/env scheme

(unless (assoc "~/code/gobble" (library-directories))
  (library-directories (cons "~/code/gobble" (library-directories))))

(import (gobble) (only (euler) shuffle))

(random-seed (time-nanosecond (current-time)))
(random-seed (random (time-nanosecond (current-time))))

(let* ((dice (if (string=? (cadr (command-line)) "5x5")
                 dice-5x5
                 (list-head (shuffle dice-5x5) 16)))
       (board (list->string (roll dice))))
  (display-ln board)
  (for-all display-ln (gobble board)))


