;; AAEEGN
;; ELRTTY
;; AOOTTW
;; ABBJOO
;; EHRTVW
;; CIMOTU
;; DISTTY
;; EIOSST
;; DELRVY
;; ACHOPS
;; HIMNQU
;; EEINSU
;; EEGHNW
;; AFFKPS
;; HLNNRZ
;; DEILRX

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

(define size
  (isqrt (length *DICE*)))

(define (roll)
  (map (lambda (die)
         (string-ref die (random (string-length die))))
       (shuffle *DICE*)))

(define (adjacent xy)
  (let ((x (car xy)) (y (cdr xy)))
    (let ((x-1 (1- x)) (x+1 (1+ x)) (y-1 (1- y)) (y+1 (1+ y)))
      `((,x-1 . ,y-1) (,x-1 . ,y) (,x-1 . ,y+1)
        (,x   . ,y-1)             (,x   . ,y+1)
        (,x+1 . ,y-1) (,x+1 . ,y) (,x+1 . ,y+1)))))

(define (board-size board)
  (vector-length board))

(define (board-indices board)
  (append-map (lambda (j)
                (map (lambda (i)
                       `((,i . ,j)))
                     (iota (board-size board))))
              (iota (board-size board))))

(define string->board
  (compose list->board string->list))

(define (list->board letters)
  (define n (isqrt (length letters)))
  (define board (make-vector n))
  (assert (= (* n n) (length letters)))
  (do ((i 0 (1+ i))
       (letters (map char-upcase letters) (list-tail letters n)))
      ((= i n) board)
    (vector-set! board i (list->string (list-head letters n)))))

(define (board!)
  (list->board (roll)))

(define (display-ln object)
  (display object) (newline))

(define (display-row row)
  (put-char (current-output-port) #\|)
  (for-all (lambda (char)
             (put-char (current-output-port) char)
             (put-char (current-output-port) #\|))
           (string->list row))
  (newline))

(define (display-board board)
  (define width (1+ (* 2 (vector-length board))))
  (display-ln (make-string width #\-))
  (vector-for-each display-row board)
  (display-ln (make-string width #\-)))
