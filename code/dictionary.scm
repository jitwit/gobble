(define (get-collins-word-list)
  (with-input-from-file "input/collins.txt"
    (lambda ()
      (let loop ((x (read)) (words '()))
        (if (eof-object? x)
            words
            (loop (read) (cons (string-upcase (symbol->string x)) words)))))))

(define collins
  (call/cc
   (lambda (k)
     (with-exception-handler
         (lambda (e)
           (let ((T (dictionary->trie (get-collins-word-list))))
             (store-trie T "input/trie.fasl")
             (k T)))
       (lambda ()
         (fetch-trie "input/trie.fasl"))))))

(define (prefix? word)
  (trie-prefix? word collins))

(define (word? word)
  (trie-member? word collins))
