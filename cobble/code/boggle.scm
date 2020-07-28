(define-record-type path
  (fields letters spot mask tree))

(define (tree-step x T)
  (and T (t:lookup-with-default (char->integer x) #f (trie-tries T))))

(define char-substitutes-table
  (fold-right (lambda (char substitutes table)
		(t:insert (char->integer char)
			  substitutes
			  table))
	      t:empty
	      (string->list "EeCcAaOo")
	      '((#\E #\É #\È)
		(#\e #\é #\è)
		(#\C #\Ç)
		(#\c #\ç)
		(#\A #\À)
		(#\a #\à)
		(#\O #\Ô)
		(#\o #\ô))))

(define (char-substitutes char)
  (t:lookup-with-default (char->integer char)
			 `(,char)
			 char-substitutes-table))

;; currently uses ints as bit sets. means maximum board size is 7x7
;; until i switch to intsets or bitvectors or something. assumes board
;; is string with perfect square length.
(define (boggle-search board dictionary)
  (define G
    (board-graph (isqrt (string-length board))))
  (define word-list
    (make-hashtable string-hash string=?))
  (define (expand-path path j)
    (let ((x (string-ref board j)))
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
  (do ((j 0 (fx1+ j)))
      ((fx= j (string-length board))
       (sort (lambda (S1 S2)
	       (let ((S1 (car S1)) (S2 (car S2)))
		 (or (< (string-length S1) (string-length S2))
		     (and (= (string-length S1) (string-length S2))
			  (string<? S1 S2)))))
	     (filter (lambda (word)
		       (fx< 2 (string-length (car word))))
		     (vector->list (hashtable-cells word-list)))))
    (walk (expand-path (make-path '() 0 0 (dictionary-trie dictionary)) j))))

(define (gobble board)
  (boggle-search board collins))

(define score-vector
  '#vfx(0 0 0 1 1 2 3 5 11))

(define (score-word word)
  (fxvector-ref score-vector (fxmin 8 (string-length word))))

(define (score-word-list word-list)
  (fold-left (lambda (score word)
	       (fx+ score (score-word word)))
	     0
	     word-list))
