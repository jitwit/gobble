
(define (nub words)
  (define seen (make-hashtable string-hash string=?))
  (define (seen? word)
    (hashtable-ref seen word #f))
  (fold-right (lambda (word words)
                (if (not (seen? word))
                    (cons word words)
                    words))
              '()
              words))

