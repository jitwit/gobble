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
     (dict->trie (get-word-list)))))
