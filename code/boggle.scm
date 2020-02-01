(define (boggle board)
  (define size (board-size board))
  (define (ref xy)
    (string-ref (vector-ref board (car xy)) (cdr xy)))
  (define (indices->string ixs)
    (list->string (map ref (reverse ixs))))
  (define (expand words)
    (define (on-board? xy)
      (and (< -1 (car xy) size) (< -1 (cdr xy) size)))
    (define (expand-word word)
      (filter-map (lambda (xy)
                    (and (on-board? xy)
                         (not (member xy word))
                         (trie-prefix?
                          (indices->string (cons xy word)) *DICTIONARY*)
                         (cons xy word)))
                  (adj (car word))))
    (append-map expand-word words))
  (let walk ((ok (expand (expand start-indices))) (words '()))
    (if (null? ok)
        (let ((seen (make-hashtable string-hash string=?)))
          (sort (lambda (A B)
                  (or (< (string-length A) (string-length B)) (string<? A B)))
                (filter-map (lambda (word)
                              (let ((word (indices->string word)))
                                (and (trie-member? word *DICTIONARY*)
                                     (not (hashtable-ref seen word #f))
                                     (hashtable-set! seen word #t)
                                     word)))
                            words)))
        (walk (expand ok) (append ok words)))))
