(unless (assoc "code" (library-directories))
  (library-directories (cons "code" (library-directories))))

(import (trie)
        (only (euler) shuffle)
        (only (srfi :1) filter-map append-map))

(define (get-word-list)
  (with-input-from-file "input/dict.txt"
    (lambda ()
      (let loop ((x (read)) (words '()))
        (if (eof-object? x)
            words
            (loop (read) (cons (string-upcase (symbol->string x)) words)))))))

(define *DICTIONARY*
  (time
   (begin
     "preprocessing the word list..."
     (dictionary->trie (get-word-list)))))

(define src-files
  '("code/board.scm"
    "code/boggle.scm"))

(parameterize ((optimize-level 3))
  (for-all load src-files))

(define (run)
  (define board (board! size))
  (for-all display-ln (boggle board))
  (newline) (display-board board))

(define example-board
  '#("FXIE"
     "AMLO"
     "EWBX"
     "ASTU"))

(define demo-board
  '#("DEMO"
     "DEMO"
     "DEMO"
     "DEMO"))
