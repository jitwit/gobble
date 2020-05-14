(random-seed (time-nanosecond (current-time)))
(random-seed (random (time-nanosecond (current-time))))

(define N
  (if (null? (cdr (command-line)))
      1
      (with-input-from-string (cadr (command-line)) read)))

(define words
  (make-hashtable string-hash string=?))

(define (freq)
  (do ((i 0 (1+ i)))
      ((= i N)
       (vector-for-each (lambda (w.n)
                          (display (car w.n))
                          (display #\space)
                          (display (cdr w.n))
                          (newline))
                        (vector-sort (lambda (x y)
                                       (>= (cdr x) (cdr y)))
                                     (hashtable-cells words))))
    
    (for-all (lambda (word)
               (hashtable-update! words word fx1+ 0))
             (gobble (list->string (roll))))))

(define (possible-combos letters)
  (filter-map (lambda (ijk)
                (let ((word (list->string (map integer->char ijk))))
                  (and (definition word) word)))
              (nub-equal (permutations letters))))

(define (word-combos word)
  (possible-combos (map char->integer (string->list word))))

(define (eq-len-combos word)
  (define n (string-length word))
  (filter (compose (curry = n) string-length) (anagrams word)))

(define (3x3s)
  (define combos (make-hash-table))
  (define (show-defs words)
    (format #t "--- ~a ---~%" words))
  (define (report)
    (for-all show-defs (hashtable-ref combos 3 '()))
    (for-all show-defs (hashtable-ref combos 4 '()))
    (for-all show-defs (hashtable-ref combos 5 '())))
  (do ((i 97 (1+ i)))
      ((> i 122)
       ;;        (report)
       combos)
    (do ((j i (1+ j)))
        ((> j 122))
      (do ((k j (1+ k)))
          ((> k 122))
        (let* ((ijk (list i j k))
               (words (possible-combos ijk))
               (n (length words)))
          (when (< 0 n)
            (hashtable-update! combos
                               (length words)
                               (lambda (cs)
                                 (cons words cs))
                               '())))))))

(define (dice-letter-freq)
  (let* ((chars (string->list (apply string-append dice-4x4)))
         (grouped (group-with char=? (sort char<? chars)))
         (letters (map car grouped))
         (freqs (map length grouped))
         (n (apply + freqs)))
    (for-all (lambda (letter freq)
               (format #t "~a ~,2f~%" letter (/ freq n 1/100)))
             letters
             freqs)))

(define dict
  (get-collins-word-list))

(define (n-letter-words n)
  (filter (compose (curry = n) string-length) dict))

(define (string-sort s)
  (list->string (sort char<? (string->list s))))

(define (order-words words)
  (group-with (lambda (s t)
                (string=? (string-sort s) (string-sort t)))
              (sort (lambda (s t)
                      (string<? (string-sort s) (string-sort t)))
                    words)))

(define (useful-combos n amount)
  (sort-on length
           (filter (compose (curry <= amount) length)
                   (order-words (n-letter-words n)))))
