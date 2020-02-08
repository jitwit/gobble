#!/usr/bin/env scheme

(unless (assoc "~/code/gobble" (library-directories))
  (library-directories (cons "~/code/gobble" (library-directories))))

(import (gobble))

(random-seed (time-nanosecond (current-time)))
(random-seed (random (time-nanosecond (current-time))))

(let ((board (list->string (roll))))
  (display-ln board)
  (for-all display-ln (gobble board)))

