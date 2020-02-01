(define B
  (board! size))

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
  (let loop ((ixs (expand ixs0)) (words '()))
    (let-values (((bad ok) (partition (bad? board) (expand ixs))))
      (if (null? ok)
          (map (lambda (word)
                 (ixs->guess board word))
               words)
          (loop ok (append (filter (word? board) ok) words))))))
