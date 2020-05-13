#lang racket

(require web-server/servlet
         web-server/servlet-env
         net/rfc6455)

;;;; SETTINGS
; defaults are development ports? idk...
(define gobble-port
  (make-parameter 9009))
(define gobble-dir
  (make-parameter "/Users/jrn/code/gobble/garcon"))

;;;; HOME
(define (gobbler/http req)
  (response/xexpr
   `(html
     (head
      (title "gobble")
      (link ((rel "stylesheet") (href "static/gobble.css")))
      ;; (script ((src "static/jquery-3.4.1.slim.js")))
      ;; (script ((src "static/gobble.js")))
      )
     (body
      (h1 "GOBBLE")
      (p ,(string-append "I'm alive: " (number->string (current-seconds))))
      (div ((class "row"))
           (div ((class "column"))
                (div ((id "gobble"))))
           (div ((class "column"))
                (div ((id "timer")))
                (form ((id "mush"))
                      (input ((autocomplete "off")
                              (spellcheck "off")
                              (type "text")
                              (id "scratch")))
                      (input ((type "submit")
                              (value "mush!")))))
           (ul ((id "submissions")))
           (div ((id "pinou")))
           (div ((class "column"))
                (div ((id "twitter"))
                     (div ((id "tweets"))))
                (form ((id "tweet"))
                      (input ((type "text")
                              (autocomplete "off")
                              (id "scribble")))
                      (input ((type "submit")
                              (hidden "")))))
           (div ((class "row"))
                (div ((class "column"))
                     (div ((id "solution"))))
                (div ((id "scores")))))))))


;;;; GAME SERVER
(define (websocket-server)
  (ws-serve #:port (sub1 (gobble-port)) (lambda (c s) (ws-send! c "hiho"))))

(define (http-server)
  (serve/servlet gobbler/http
                 #:port (gobble-port)
                 #:extra-files-paths `(,(gobble-dir))
                 #:servlet-path "/"
                 #:servlet-current-directory (gobble-dir)
                 #:command-line? #t))

(define thread/wss
  (thread websocket-server))
(define thread/http
  (thread http-server))

(define (main)
  (let loop ()
    (display (current-inexact-milliseconds)) (newline)
    (sleep 2)
    (loop)))

(main)

