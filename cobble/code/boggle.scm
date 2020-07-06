(define-record-type path
  (fields letters spot mask tree))

(define (tree-step x T)
  (and T (t:lookup-with-default (char->integer x) #f (trie-tries T))))

;; currently uses ints as bit sets. means maximum board size is 7x7
;; until i switch to intsets or bitvectors or something. assumes board
;; is string with perfect square length.
(define (dfs B D)
  (define G
    (board-graph (isqrt (string-length B))))
  (define word-list
    (make-hashtable string-hash string=?))
  (define (expand-path path j)
    (let ((x (string-ref B j)))
      (if (char=? x #\Q)
	  (make-path (cons* #\U #\Q (path-letters path))
		     j
		     (fxlogbit1 j (path-mask path))
		     (tree-step #\U (tree-step #\Q (path-tree path))))
	  (make-path (cons x (path-letters path))
		     j
		     (fxlogbit1 j (path-mask path))
		     (tree-step x (path-tree path))))))
  (define (walk path)
    (when (path-tree path)
      (when (trie-element (path-tree path))
	(hashtable-set! word-list
			(list->string (reverse (path-letters path)))
			(trie-element (path-tree path))))
      (for-each (lambda (j)
		  (unless (fxbit-set? (path-mask path) j)
		    (walk (expand-path path j))))
		(vector-ref G (path-spot path)))))
  (for-each (lambda (j)
	      (walk (expand-path (make-path '() 0 0 D) j)))
	    (iota (string-length B)))
  (filter (lambda (word)
	    (< 2 (string-length (car word))))
	  (sort-on (compose string-length car)
		   (sort (lambda (x y)
			   (string<? (car x) (car y)))
			 (vector->list (hashtable-cells word-list))))))

(define (gobble board)
  (dfs board collins))

(define score-vector
  '#vfx(0 0 0 1 1 2 3 5 11))

(define (score-word word)
  (fxvector-ref score-vector (fxmin 8 (string-length word))))

(define (score-word-list word-list)
  (fold-left (lambda (score word)
	       (fx+ score (score-word word)))
	     0
	     word-list))
