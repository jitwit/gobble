(define (add-lib path)
  (unless (assoc path (library-directories))
    (library-directories (cons path (library-directories)))))

(add-lib ".")

(import (dictionary)
        (gobble)
        (euler)
        (dictionary)
        (trie)
        (only (srfi :1) append-map filter-map))

(define (dump-collins)
  (define out-file "share/clns.txt")
  (delete-file out-file)
  (with-output-to-file out-file
    (lambda ()
      (define out (current-output-port))
      (define (put-def def)
        (display (car def) out)
        (display "@" out)
        (display (cdr def) out)
        (newline))
      (for-all put-def (reverse (get-collins))))))

(define (try-parallel)
  (define b (roll dice-4x4))
  (display (time (length (gobble b))))
  (newline)
  (display (time (length (pobble b))))
  (newline))

;; (define (get-dela-fr) ;; danger - large file
;;   (define dict "input/dela-fr-public-u8.dic.xml")
;;   (with-input-from-file dict
;;     (lambda ()
;;       (ssax:xml->sxml (current-input-port) '()))))

;; (define (read-dela tree)
;;   (map cadr (cadr tree)))

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

(define (completes prefix cutoff)
  (map string-downcase
       (filter (compose (curry >= cutoff) string-length)
               (completions prefix))))
