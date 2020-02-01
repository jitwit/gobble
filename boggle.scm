(load "trie.scm")

(define size 4)

(define D
  (time
   (dict->trie (get-words))))

(define (board! n)
  (define board
    (make-vector n))
  (do ((i 0 (1+ i)))
      ((= i n) board)
    (vector-set! board i
                 (list->string
                  (map (lambda (x)
                         (integer->char (+ 65 (random 26))))
                       (make-list n 0))))))

(define B
  (board! size))

(define (ref B)
  (lambda (xy)
    (string-ref (vector-ref B (car xy)) (cdr xy))))

(define ixs0
  (apply append
         (map (lambda (j)
                (map (lambda (i)
                       (list (cons i j)))
                     (iota size)))
              (iota size))))

(define (adj xy)
  (let ((x (car xy)) (y (cdr xy)))
    (let ((x-1 (1- x)) (x+1 (1+ x)) (y-1 (1- y)) (y+1 (1+ y)))
      `((,x-1 . ,y-1) (,x-1 . ,y) (,x-1 . ,y+1)
        (,x . ,y-1)               (,x . ,y+1)
        (,x+1 . ,y-1) (,x+1 . ,y) (,x+1 . ,y+1)))))

(define (on-board? xy)
  (and (< -1 (car xy) size)
       (< -1 (cdr xy) size)))

(define (expand-word word)
  (map (lambda (xy)
         (cons xy word))
       (adj (car word))))

(define (expand words)
  (filter valid? (apply append (map expand-word words))))

(define (valid? ixs)
  (and (on-board? (car ixs))
       (not (member (car ixs) (cdr ixs)))))

(define (ixs->guess board ixs)
  (list->string (map (ref board) ixs)))

(define (bad? board)
  (lambda (ixs)
    (not (trie-prefix? (ixs->guess board ixs) D))))

(define (word? board)
  (lambda (ixs)
    (trie-member? (ixs->guess board ixs) D)))

(define (solve board)
  (let loop ((ixs ixs0) (words '()))
    (let-values (((bad ok) (partition (bad? board) (expand ixs))))
      (display (list (length ok) (length bad))) (newline)
      (if (null? ok)
          (map (lambda (word)
                 (ixs->guess board word))
               words)
          (loop ok (append (filter (word? board) ok) words))))))
