(define (gobble1 board)
  (time (begin (gobble board) (void))))

(define (do-gobble N)
  (do ((i 0 (fx1+ i)))
      ((fx= i N))
    (gobble1 example3)))

(define example1 "SKDSMAHLETROEUSP")
(define example2 "ELIHYWOTXSERINAT")
(define example3 "LGROSEDENSTAEFUTERSFMETCO")
(define (random-board n)
  (define n^2 (square n))
  (roll (list-head (apply append (make-list (ceiling (/ n^2 25)) dice-5x5))
		   n^2)))

(define (run-bench N)
  (define out "../report/bench.txt")
  (when (file-exists? out)
    (delete-file out))
  (transcript-on out)
  (do-gobble N)
  (transcript-off))

(define-record-type state
  (fields dice board score))

(define (string-head S)
  (string-ref S 0))

(define (configuration->board dice)
  (list->string (map string-head dice)))

(define swaps
  (combinations (iota 16) 2))

(define (random-swap)
  (list-ref swaps (random 120)))

(define (dice->state dice)
  (let* ((board (configuration->board (t:tree->items dice)))
	 (score (score-word-list (gobble board))))
    (make-state dice board score)))

(define (swap-two-dice state)
  (let* ((xy (random-swap)) (tree (state-dice state)))
    (dice->state
     (t:insert (cadr xy)
	       (t:lookup-with-default (car xy) #f tree)
	       (t:insert (car xy)
			 (t:lookup-with-default (cadr xy) #f tree)
			 tree)))))

(define (permute-single-dice state)
  (dice->state
   (t:modify (lambda (dice)
	       (list->string (shuffle (string->list dice))))
	     (random 16)
	     (state-dice state))))

(define (neighbor state)
  (if (< (random 0.5))
      (permute-single-dice state)
      (swap-two-dice state)))

(define (boggle-schedule N)
  (lambda (t)
    (/ (- N t) N)))

;; state will be configuration of dice. swaps positions of dice, or
;; order for individidual dice
;; GNISETRPSEACDBLS 11297 ; hah!
;; LMSDIAEOSTRNHEIG 12067
;; TGEDENITSRASOBEL 12221
;; STERLIRGMANETESD 12342
;; HOSAMNRTEAIESTLS 12386
;; MLPEAIASSNRTTEED 12705
;; PSLMEAIARNTRGESO 12848

;; SERSPATGLINESERS 15554
;; whatever dice: ARESSTIMENALGRES 14157
;; STLPEIAEDNRSSETP 15147

(define (sort-string S)
  (list->string (sort char<? (string->list S))))
(define (string-has? c)
  (lambda (S)
    (and (memv c (string->list S)) #t)))

(define (random-dice)
  ;; (map (lambda (n) "ABCDEFGHIKLMNPRST") (iota 16))
  (shuffle dice-4x4))

(define (simulate-annealing schedule)
  (define state
    (dice->state (fold-right t:insert t:empty (iota 16) (random-dice))))
  (do ((t 1 (1+ t))
       (T (schedule 1) (schedule t)))
      ((=~ T 0)
       (values (state-board state) (state-score state)))
    (let* ((next (neighbor state))
	   (delta (- (state-score next) (state-score state))))
      (meta-cond
       ((< 2 (verbosity))
	(when (= 0 (mod t 100))
	  (format #t "~a ~a ~a~%" t (state-board state) (state-score state))))
       ((< 1 (verbosity))
	(when (= 0 (mod t 250))
	  (format #t "~a ~a ~a~%" t (state-board state) (state-score state)))))
      (if (< 0 delta)
	  (set! state next)
	  (when (< (random 1.0) (exp (/ delta T)))
	    (set! state next))))))

(define (multiple-annealings schedule N)
  (define-values (board score) (simulate-annealing schedule))
  (do ((i 0 (1+ i)))
      ((= i N) (values board score))
    (format #T "~%STARTING ROUND ~a ~a ~a~%" i board score)
    (let-values (((board* score*) (simulate-annealing schedule)))
      (when (< score score*)
	(set! board board*)
	(set! score score*)))))
