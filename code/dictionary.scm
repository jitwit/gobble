(define (parse-definition line)
  (define (strip-comma s)
    (do ((j (1- (string-length s)) (1- j)))
        ((or (zero? j) (not (char=? (string-ref s j) #\,)))
         (substring s 0 (1+ j)))))
  (define (defined-word? word)
    (do ((j 0 (1+ j))
         (ok #t (and ok (char-upper-case? (string-ref word j)))))
        ((= j (string-length word)) ok)))
  (let loop ((tokens (string-tokenize line)) (words '()))
    (let ((word (car tokens)))
      (if (defined-word? (strip-comma word))
          (loop (cdr tokens) (cons word words))
          (map (lambda (word)
                 (cons (strip-comma word) (string-join tokens " ")))
               words)))))

(define (get-collins-word-list)
  (with-input-from-file "input/collins.txt"
    (lambda ()
      (let loop ((x (read)) (words '()))
        (if (eof-object? x)
            words
            (loop (read) (cons (string-upcase (symbol->string x)) words)))))))

(define (get-collins)
  (with-input-from-file "input/definitions.txt"
    (lambda ()
      (define in (current-input-port))
      (let loop ((x (get-line in)) (words '()))
        (if (eof-object? x)
            words
            (loop (get-line in) (append (parse-definition x) words)))))))

(define collins
  (call/cc
   (lambda (k)
     (with-exception-handler
         (lambda (e)
           (let ((T (dictionary->trie (get-collins))))
             (store-trie T "input/trie.fasl")
             (k T)))
       (lambda ()
         (fetch-trie "input/trie.fasl"))))))

(define (prefix? word)
  (trie-prefix? word collins))

(define (word? word)
  (trie-member? word collins))

(define (definition word)
  (lookup (string-upcase word) collins))

(define (suffixes prefix)
  (map (lambda (path)
         (list->string (map integer->char path)))
       (trie-paths (trie-ref collins prefix))))

(define (completions prefix)
  (map (lambda (suffix)
         (string-append prefix suffix))
       (suffixes prefix)))
