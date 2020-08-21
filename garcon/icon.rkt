#lang racket

(require pict
         racket/draw
         file/convertible)

(define r '(255 13 77))
(define y '(255 220 160))
(define b '(17 126 255))

(define (letter l)
  (text (string l) '(bold . "APL385 Unicode") 20))

(define flav.ico
  (cc-superimpose (colorize (filled-rectangle 32 32) y)
                  (hc-append (colorize (letter #\B) r)
                             (colorize (letter #\B) b))))

(define (main)
  (define icon.png "static/icon.png")
  (when (file-exists? icon.png)
    (delete-file icon.png))
  (with-output-to-file icon.png
    (lambda ()
      (display
       (convert flav.ico 'png-bytes)))))

flav.ico
