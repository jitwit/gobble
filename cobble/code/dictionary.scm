(define-record-type dictionary
  (fields language trie))

(define (parse-definition line)
  (string-tokenize line (char-set-complement (char-set #\tab #\return))))

(define (get-collins-word-list)
  (with-input-from-file "share/collins.txt"
    (lambda ()
      (let loop ((x (read)) (words '()))
        (if (eof-object? x)
            words
            (loop (read) (cons (string-upcase (symbol->string x)) words)))))))

(define (get-yawl)
  (with-input-from-file "share/yawl.txt"
    (lambda ()
      (let loop ((x (read)) (words '()))
        (if (eof-object? x)
            words
            (loop (read) (cons (string-upcase (symbol->string x)) words)))))))

(define (get-collins)
  (with-input-from-file "share/definitions.txt"
    (lambda ()
      (define in (current-input-port))
      (let loop ((x (get-line in)) (words '()))
        (if (eof-object? x)
            words
            (loop (get-line in) (cons (apply cons (parse-definition x)) words)))))))

(define collins
  (make-dictionary 'english
		   (call/cc
		    (lambda (k)
		      (with-exception-handler
			  (lambda (e)
			    (let ((T (dictionary->trie (get-collins))))
			      (store-trie T "share/trie.fasl")
			      (k T)))
			(lambda ()
			  (fetch-trie "share/trie.fasl")))))))

(define (prefix? word)
  (trie-prefix? word (dictionary-trie collins)))

(define (word? word)
  (trie-member? word (dictionary-trie collins)))

(define (definition word)
  (lookup (string-upcase word) (dictionary-trie collins)))

(define (suffixes prefix)
  (cond ((lookup-string collins prefix)
         => (lambda (subtrie)
              (map (lambda (path)
                     (list->string (map integer->char path)))
                   (trie-paths subtrie))))
        (else '())))

(define (completions prefix)
  (map (lambda (suffix)
         (string-append prefix suffix))
       (suffixes (string-upcase prefix))))

(define (remv1 x xs)
  (define (aux xs)
    (cond ((null? xs) '())
          ((eqv? (car xs) x) (cdr xs))
          (else (cons (car xs) (aux (cdr xs))))))
  (aux xs))

(define (anagrams word)
  (define anas (make-hashtable string-hash string=?))
  (define (aux T path letters)
    (when (trie-element T)
      (hashtable-set! anas (string-downcase (list->string (reverse path))) #t))
    (for-all (lambda (letter)
               (let ((y.T (lookup-char T letter)))
                 (when (pair? y.T)
                   (aux (cdr y.T) (cons letter path) (remv1 letter letters)))))
             (nub-eq letters)))
  (aux collins '() (string->list (string-upcase word)))
  (sort (lambda (x y)
          (let ((n.x (string-length x)) (n.y (string-length y)))
            (or (< n.x n.y)
                (and (= n.x n.y)
                     (string<? x y)))))
        (vector->list (hashtable-keys anas))))
