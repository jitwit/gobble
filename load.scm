(import (prefix (patricia) t:))

(print-gensym #f)

(define src-files
  '("code/dice.scm"
    "code/board.scm"
    "code/trie.scm"
    "code/boggle.scm"
    "code/words.scm"))

(for-all load src-files)

(define (get-words)
  (with-input-from-file "input/words.txt"
    (lambda ()
      (let loop ((x (read)) (words '()))
        (if (eof-object? x)
            words
            (loop (read) (cons (string-upcase (symbol->string x)) words)))))))

(define *DICTIONARY*
  (time
   (dict->trie (get-words))))

(define (run)
  (define board (board! size))
  (for-all display-ln (reverse (nub (solve board))))
  (newline)
  (display-board board))

