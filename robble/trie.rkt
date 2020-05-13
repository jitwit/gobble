#lang racket

(provide trie?
         trie-element
         trie-tries
         trie-ref* ; list
         lookup
         lookup-char
         lookup-string
         trie-prefix?
         trie-member?
         dictionary->trie
         store-trie
         fetch-trie
         trie-paths)

(require (only-in srfi/1
                  append-map)
         (prefix-in t: patricia)
         (except-in rnrs cons pair? car cdr fold-right)
         racket/fasl)

(define fasl-read fasl->s-exp)
(define fasl-write s-exp->fasl)
(define fold-right foldr)

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
          (make-trie #f (t:singleton (char-ref s j) (aux (+ j 1))))))
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

(define (lookup-string T s)
  (let ((n (string-length s)))
    (define (aux j T)
      (cond ((= j n) T)
            ((t:lookup (char-ref s j) (trie-tries T))
             => (lambda (x.ts) (aux (+ j 1) (cdr x.ts))))
            (else #f)))
    (aux 0 T)))

(define (trie-ref* T xs)
  (define (aux T xs)
    (cond ((null? xs) T)
          ((t:lookup (char->integer (car xs)) (trie-tries T))
           => (lambda (x.ts) (aux (cdr x.ts) (cdr xs))))
          (else #f)))
  (aux T xs))

(define (lookup-char T x)
  (t:lookup (char->integer x) (trie-tries T)))

(define (lookup s T)
  (cond ((lookup-string T s) => trie-element)
        (else #f)))

(define (trie-member? s T)
  (let ((T (lookup-string T s)))
    (and (trie? T) (trie-element T) #t)))

(define (trie-prefix? s T)
  (and (lookup-string T s) #t))

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
  
  
