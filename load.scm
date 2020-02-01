(load "board.scm")
(load "trie.scm")
(load "boggle.scm")

(define D
  (time
   (dict->trie (get-words))))

(define (run)
  (define board (board! size))
  (for-all display-ln (reverse (solve board)))
  (newline)
  (display-board board))

