(define-record-type trie
  (fields element tries))

(define (char-ref s j)
  (char->integer (string-ref s j)))

(define empty-trie
  (make-trie #f t:empty))

(define (singleton s v)
  (let ((n (string-length s)))
    (define (aux j)
      (if (= j n)
          (make-trie v t:empty)
          (make-trie #f (t:singleton (char-ref s j) (aux (1+ j))))))
    (aux 0)))

(define (merge-tries S T)
  (cond ((t:empty? S) T)
        ((t:empty? T) S)
        (else
         (make-trie (or (trie-element S) (trie-element T))
                    (t:merge-with merge-tries
                                  (trie-tries S)
                                  (trie-tries T))))))

(define (dictionary->trie definitions)
  (fold-right (lambda (x.y t)
                (merge-tries (singleton (car x.y) (cdr x.y)) t))
              (make-trie #f t:empty)
              definitions))

(define (trie-ref T s)
  (let ((n (string-length s)))
    (define (aux j T)
      (cond ((= j n) T)
            ((t:lookup (char-ref s j) (trie-tries T))
             => (lambda (x.ts) (aux (1+ j) (cdr x.ts))))
            (else #f)))
    (aux 0 T)))

(define (lookup s T)
  (cond ((trie-ref T s) => trie-element)
        (else #f)))

(define (trie-member? s T)
  (let ((T (trie-ref T s)))
    (and (trie? T) (trie-element T) #t)))

(define (trie-prefix? s T)
  (and (trie-ref T s) #t))

(define (trie-paths T)
  (let ((ts (t:tree->alist (trie-tries T))))
    (if (null? ts)
        '(())
        (append-map (lambda (k.v)
                      (map (lambda (t)
                             (cons (car k.v) t))
                           (trie-paths (cdr k.v))))
                    ts))))

(define (store-trie obj file)
  (when (file-exists? file)
    (delete-file file))
  (let ((out (open-file-output-port file)))
    (fasl-write obj out)
    (close-output-port out)))

(define (fetch-trie file)
  (let ((in (open-file-input-port file)))
    (let ((obj (fasl-read in)))
      (close-input-port in)
      (assert (trie? obj))
      obj)))
