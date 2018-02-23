(library (libra server tiny)
  (export tiny:run)
  (import 
    (scheme)
    (socket socket)
    (libra server http-cgi))
  
  ;; 是否多线程
  (define threads?
    (char=? #\t (string-ref (symbol->string (machine-type)) 0)))

  (define tiny:run
    (case-lambda
      ([proc port] 
        (tiny:run proc port "127.0.0.1"))
      ([proc port ip] 
        (let ([server (socket:socket AF_INET SOCK_STREAM IPPROTO_IP)])
          (socket:bind server AF_INET ip port)
          (socket:listen server)
          (let loop ([client (socket:accept server)])
            (when (>= client 0)
              (if threads?
                (fork-thread
                  (lambda ()
                    (server-proc proc client)))
                (server-proc proc client))
              (loop (socket:accept server))))
          (socket:close server)
          (socket:cleanup)))))

    (define server-proc
      (lambda (proc socket)
        (http:serve-query proc socket)
        (socket:close socket)))
)

; (import 
;   (socket socket)
;   (libra server http-cgi)
;   (libra server tiny))

; (define port 8211)
; (define ip "127.0.0.1")

; (define serve-proc
; 	(lambda (request-line query-string header)
;     (printf "HTTP=~a\n" request-line)
;     (printf "path=~a\n" (cadr request-line))
;     (string-append
;       (http:content
;       '(("Content-Type" . "text/html"))
;       "<div>test</div>"))))

; (tiny:run serve-proc port ip)