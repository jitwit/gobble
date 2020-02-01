(define *DICE*
  '("NAEAEG"
    "EGNWEH"
    "CSOAHP"
    "LVERDY"
    "TOATOW"
    "PKAFFS"
    "HRVTWE"
    "HQUMNI"
    "EITSSO"
    "RLTYET"
    "TITSYD"
    "LXEDIR"
    "TOIMCU"
    "BAOBOJ"
    "NLNHZR"
    "ENSIEU"))

(define size (isqrt (length *DICE*)))

(define (roll die)
  (string-ref die (random (string-length die))))

(define start-indices
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
  (define board (make-vector n))
  (do ((i 0 (1+ i))
       (letters (map roll *DICE*) (list-tail letters size)))
      ((= i n) board)
    (vector-set! board i (list->string (list-head letters size)))))

(define (display-ln object)
  (display object) (newline))

(define (display-board board)
  (vector-for-each display-ln board))
