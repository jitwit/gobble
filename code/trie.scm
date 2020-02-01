(define-record-type trie
  (fields element tries))

(define (cref s j)
  (char->integer (string-ref s j)))

(define (singleton s)
  (let ((n (string-length s)))
    (define (aux j)
      (if (= j n)
          (make-trie s t:empty)
          (make-trie #f (t:singleton (cref s j) (aux (1+ j))))))
    (aux 0)))

(define (merge-tries S T)
  (cond ((t:empty? S) T)
        ((t:empty? T) S)
        (else
         (make-trie (or (trie-element S) (trie-element T))
                    (t:merge-with merge-tries
                                  (trie-tries S)
                                  (trie-tries T))))))

(define (dictionary->trie strings)
  (define (aux xs)
    (cond ((null? xs) (make-trie #f t:empty))
          ((null? (cdr xs)) (car xs))
          (else (merge-tries (merge-tries (car xs) (cadr xs))
                             (aux (cddr xs))))))
  (aux (map singleton strings)))

(define (dict->trie word-list)
  (fold-right (lambda (x t)
                (merge-tries (singleton x) t))
              (make-trie #f t:empty)
              word-list))

(define (lookup-prefix s T)
  (let ((n (string-length s)))
    (define (aux j T)
      (cond ((= j n)
             (and (or (trie-element T) (not (t:empty? (trie-tries T))))
                  T))
            ((t:lookup (cref s j) (trie-tries T)) =>
             (lambda (x.ts)
               (aux (1+ j) (cdr x.ts))))
            (else #f)))
    (aux 0 T)))

(define (trie-member? s T)
  (let ((T (lookup-prefix s T)))
    (and (trie? T)
         (trie-element T)
         #t)))

(define (trie-prefix? s T)
  (and (lookup-prefix s T)
       #t))


