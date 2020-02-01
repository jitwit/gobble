#!chezscheme

(eval-when (compile load)
  (optimize-level 3))

(library (trie)
  (export trie?
          lookup-prefix
          trie-prefix?
          trie-member?
          dictionary->trie
          store-trie
          fetch-trie)
  (import (prefix (patricia) t:)
          (chezscheme))

  (define-record-type trie
    (fields element tries))

  (define (char-ref s j)
    (char->integer (string-ref s j)))

  (define (singleton s)
    (let ((n (string-length s)))
      (define (aux j)
        (if (= j n)
            (make-trie #t t:empty)
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

  (define (dictionary->trie word-list)
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
              ((t:lookup (char-ref s j) (trie-tries T))
               =>
               (lambda (x.ts)
                 (aux (1+ j) (cdr x.ts))))
              (else #f)))
      (aux 0 T)))

  (define (trie-member? s T)
    (let ((T (lookup-prefix s T)))
      (and (trie? T) (trie-element T))))

  (define (trie-prefix? s T)
    (lookup-prefix s T))

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
  
  )
