(import (prefix (patricia) t:)
        (only (euler) compose shuffle)
        (only (srfi :1) append-map filter-map)
        (only (srfi :13) string-tokenize string-join))

(for-all load
         '("code/trie.scm"
           "code/dictionary.scm"
           "code/board.scm"
           "code/boggle.scm"))

(define (run)
  (define board (board!))
  (for-all display-ln (boggle board))
  (newline) (display-board board))

(define (test)
  (define (lookup-prefix s t)
    (trie-ref t s))
  (time
   (begin
     "basic trie tests"
     (let* ((cat (singleton "cat" "cat"))
            (cat+cave (merge-tries cat (singleton "cave" "cave"))))
       (assert (lookup-prefix "" cat))
       (assert (lookup-prefix "cat" cat))
       (assert (lookup-prefix "ca" cat))
       (assert (not (lookup-prefix "at" cat)))
       (assert (not (lookup-prefix "cats" cat)))
       (assert (lookup-prefix "ca" cat+cave))
       (assert (lookup-prefix "cav" cat+cave)))))
  (time
   (begin
     "dictionary test"
     (for-all (lambda (word)
                (assert (and (prefix? word) (word? word))))
              (get-collins-word-list)))))


