(define-record-type path
  (fields letters spot mask tree))

(define (step x T)
  (and T (t:lookup-with-default (char->integer x) #f (trie-tries T))))

(define (dfs B D)
  (define G
    (board-graph (isqrt (string-length B))))
  (define word-list
    (make-hashtable string-hash string=?))
  (define (walk path)
    (when (path-tree path)
      (when (trie-element (path-tree path))
	(hashtable-set! word-list
			(list->string (reverse (path-letters path)))
			(trie-element (path-tree path))))
      (for-each (lambda (j)
		  (unless (fxbit-set? (path-mask path) j)
		    (let ((x (string-ref B j)))
		      (if (char=? x #\Q)
			  (walk (make-path (cons* #\U #\Q (path-letters path))
					   j
					   (fxlogbit1 j (path-mask path))
					   (step #\U (step #\Q (path-tree path)))))
			  (walk (make-path (cons x (path-letters path))
					   j
					   (fxlogbit1 j (path-mask path))
					   (step x (path-tree path))))))))
		(vector-ref G (path-spot path)))))
  (for-each (lambda (j)
	      (let ((x (string-ref B j)))
		(if (char=? x #\Q)
		    (walk (make-path '(#\U #\Q) j (fxlogbit1 j 0) (step #\U (step #\Q D))))
		    (walk (make-path (list x) j (fxlogbit1 j 0) (step x D))))))
	    (iota (string-length B)))
  (filter (lambda (word)
	    (< 2 (string-length (car word))))
	  (sort-on (compose string-length car)
		   (sort (lambda (x y)
			   (string<? (car x) (car y)))
			 (vector->list (hashtable-cells word-list))))))
