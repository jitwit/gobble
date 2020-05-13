#lang racket/base

(require web-server/servlet
         web-server/servlet-env
         web-server/dispatch
         net/rfc6455
         net/url)

;;;; SETTINGS
(define gobble-websocket-port
  (make-parameter 9009))
(define gobble-http-port
  (make-parameter 9009))
(define gobble-dir
  (make-parameter "/Users/jrn/code/gobble/garcon"))

;;;; HOME
(define (gobbler/http req)
  (display req) (newline)
  (response/xexpr
   `(html
     (head
      (title "gobble")
      (link ((rel "stylesheet") (href "gobble.css")))
      (script ((src "jquery-3.4.1.slim.js")))
      (script ((src "robble.js"))))
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
  (ws-serve #:port (gobble-websocket-port)
            (lambda (c s)
              (display `(hiho ,c ,s))
              (newline)
              
              )))

(define (http-server)
  (serve/servlet gobbler/http
                 #:port (gobble-http-port)
                 #:extra-files-paths `(,(string-append (gobble-dir) "/static"))
                 #:listen-ip #f
                 #:servlet-path "/boggle-bitch"
                 #:command-line? #t))

(define (main)
  (display 'hiho) (newline)
  (define thread/wss
    (thread websocket-server))
  (define thread/http
    (thread http-server))
  (let loop ()
    (sleep 0.1)
    (loop)))

(main)
