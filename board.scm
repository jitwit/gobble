(define size 4)

(define ixs0
  (apply append
         (map (lambda (j)
                (map (lambda (i)
                       (list (cons i j)))
                     (iota size)))
              (iota size))))

(define (adj xy)
  (let ((x (car xy)) (y (cdr xy)))
    (let ((x-1 (1- x)) (x+1 (1+ x)) (y-1 (1- y)) (y+1 (1+ y)))
      `((,x-1 . ,y-1) (,x-1 . ,y) (,x-1 . ,y+1)
        (,x . ,y-1)               (,x . ,y+1)
        (,x+1 . ,y-1) (,x+1 . ,y) (,x+1 . ,y+1)))))


(define (ref board)
  (lambda (xy)
    (string-ref (vector-ref board (car xy)) (cdr xy))))

(define (board! n)
  (define board
    (make-vector n))
  (do ((i 0 (1+ i)))
      ((= i n) board)
    (vector-set! board i
                 (list->string
                  (map (lambda (x)
                         (integer->char (+ 65 (random 26))))
                       (make-list n 0))))))

(define (display-ln object)
  (display object) (newline))

(define (display-board board)
  (vector-for-each display-ln board))
