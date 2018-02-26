;;; "http-cgi.scm" service HTTP or CGI requests. -*-scheme-*-
; Copyright 1997, 1998, 2000, 2001, 2003 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

(library (libra server http-cgi)
  (export
	http:read-header
	http:read-query-string
	http:status-line
	http:header
	http:content
	http:error-page
	http:serve-query
	http:read-start-line
	http:read-request-line
	html:head
	html:body)
  (import
		(scheme)
		(socket socket)
		(irregex irregex))

;;@code{(require 'http)} or @code{(require 'cgi)}
;;@ftindex http
;;@ftindex cgi

(define http:crlf (string (integer->char 13) #\newline))

(define (http:read-header port)
  (define alist '())
  (do ((line (read-line port) (read-line port)))
		((or (zero? (string-length line))
					(and (= 1 (string-length line))
							(char-whitespace? (string-ref line 0)))
					(eof-object? line))
			(if (and (= 1 (string-length line))
							(char-whitespace? (string-ref line 0)))
				(set! http:crlf (string (string-ref line 0) #\newline)))
			(if (eof-object? line) line alist))
		(let ((len (string-length line))
					(idx (string-index line #\:)))
			(if (char-whitespace? (string-ref line (+ -1 len)))
				(set! len (+ -1 len)))
			(and idx 
				(do ((idx2 (+ idx 1) (+ idx2 1)))
						((or (>= idx2 len)
									(not (char-whitespace? (string-ref line idx2))))
							(set! alist 
							(cons 
								(cons (string-ci->symbol (substring line 0 idx))
											(substring line idx2 len))
							alist)))
				)
			)
		)
	)
)

(define (http:read-query-string request-line header port)
  (case (car request-line)
    ((get head)
			(let* ((request-uri (cadr request-line))
						 (len (string-length request-uri)))
				(and 
					(> len 3)
				  (string-index request-uri #\?)
					(substring request-uri
						(+ 1 (string-index request-uri #\?))
						(if (eqv? #\/ (string-ref request-uri (+ -1 len)))
							(+ -1 len)
							len)))))
    ((post put delete)
			(let ((content-length (assq 'content-length header)))
				(and 
					content-length
					(set! content-length (string->number (cdr content-length))))
				(and 
					content-length
					(let ((str (make-string content-length #\space)))
						(do ((idx 0 (+ idx 1)))
							((>= idx content-length)
								(if (>= idx (string-length str)) 
									str
									(substring str 0 idx)))
							(let ((chr (read-char port)))
								(if (char? chr)
									(string-set! str idx chr)
									(set! content-length idx))))))))
    (else #f)))

(define (http:status-line status-code reason)
  (format "HTTP/1.0 ~a ~a~a" status-code reason http:crlf))

;;@body Returns a string containing lines for each element of @1; the
;;@code{car} of which is followed by @samp{: }, then the @code{cdr}.
(define (http:header alist)
  (string-append
		(apply
			string-append
			(map 
				(lambda (pair)
		 			(format "~a: ~a~a" (car pair) (cdr pair) http:crlf))
	    	alist))
   	http:crlf))

;;@body Returns the concatenation of strings @2 with the
;;@code{(http:header @1)} and the @samp{Content-Length} prepended.
(define (http:content alist . body)
  (define hunk (apply string-append body))
  (string-append 
		(http:header
			(cons 
				(cons 
					"Content-Length"
					(number->string  (bytevector-length (string->utf8 hunk)))) ;;(string-length hunk)
				alist))
   	hunk))

;;@body String appearing at the bottom of error pages.
(define *http:byline* #f)

;;@body @1 and @2 should be an integer and string as specified in
;;@cite{RFC 2068}.  The returned page (string) will show the @1 and @2
;;and any additional @3 @dots{}; with @var{*http:byline*} or SLIB's
;;default at the bottom.
(define (http:error-page status-code reason-phrase . html-strings)
  	(define byline
		(or
			*http:byline*
			"Libra HTTP/1.0 Server"))
	(string-append 
		(http:status-line status-code reason-phrase)
		(http:content
		  '(("Content-Type" . "text/html"))
		  (html:head (format "~a ~a" status-code reason-phrase))
		  (apply html:body
			 (append html-strings
				 (list (format "<HR>\n~a\n" byline)))))))



;;@body reads the @dfn{URI} and @dfn{query-string} from @2.  If the
;;query is a valid @samp{"POST"} or @samp{"GET"} query, then @0 calls
;;@1 with three arguments, the @var{request-line}, @var{query-string},
;;and @var{header-alist}.  Otherwise, @0 calls @1 with the
;;@var{request-line}, #f, and @var{header-alist}.
;;
;;If @1 returns a string, it is sent to @3.  If @1 returns a list
;;whose first element is an integer, then an error page with the
;;status integer which is the first element of the list and strings
;;from the list.  If @1 returns a list whose first element isn't an
;;number, then an error page with the status code 500 and strings from
;;the list.  If @1 returns #f, then a @samp{Bad Request} (400) page is
;;sent to @3.
;;
;;Otherwise, @0 replies (to @3) with appropriate HTML describing the
;;problem.
(define (http:serve-query serve-proc client-socket)
  (let* ([input-port (make-input-port (lambda x (void)) (utf8->string (socket:read client-socket)))]
		 [request-line (http:read-request-line input-port)]
		 [header (and request-line (http:read-header input-port))]
		 [query-string (and header (http:read-query-string
						request-line header input-port))]
		 [rst (http:service serve-proc request-line query-string header)])
	(socket:write client-socket (if (bytevector? rst) rst (string->utf8 rst)))))


(define (http:service serve-proc request-line query-string header)
  (cond 
		((not request-line)
			(http:error-page 400 "Bad Request."))
		((string? (car request-line))
			(http:error-page 501 "Not Implemented" (html:plain request-line)))
		((not (memq (car request-line) '(get post)))
			(http:error-page 405 "Method Not Allowed" (html:plain request-line)))
		((serve-proc request-line query-string header) =>
			(lambda (reply)
				(cond 
					((bytevector? reply)
						reply)
					((string? reply)
						(string-append (http:status-line 200 "OK")
							reply))
					((and (pair? reply) (list? reply))
						(if (number? (car reply))
							(apply http:error-page reply)
							(apply http:error-page (cons 500 reply))))
					(else (http:error-page 500 "Internal Server Error")))))
		((not query-string)
			(http:error-page 400 "Bad Request" (html:plain request-line)))
		(else
			(http:error-page 500 "Internal Server Error" (html:plain header)))))

(define (http:read-start-line port)
  (do ((line (read-line port) (read-line port)))
		((or (not (equal? "" line)) (eof-object? line)) line)))

;; @body
;; Request lines are a list of three itmes:
;;
;; @enumerate 0
;;
;; @item Method
;;
;; A symbol (@code{options}, @code{get}, @code{head}, @code{post},
;; @code{put}, @code{delete}, @code{trace} @dots{}).
;;
;; @item Request-URI
;;
;; A string.  For direct HTTP, at the minimum it will be the string
;; @samp{"/"}.
;;
;; @item HTTP-Version
;;
;; A string.  For example, @samp{HTTP/1.0}.
;; @end enumerate
(define (http:read-request-line port)
  (let ((lst (request-split (http:read-start-line port))))
		(and 
			(list? lst)
	 		(= 3 (length lst))
			 (cons (string-ci->symbol (car lst)) (cdr lst)))))

;; -------------------- * helper * --------------------

(define html:blank (string->symbol ""))

;;@body Returns a string with character substitutions appropriate to
;;send @1 as an @dfn{plain-text}.
(define (html:plain txt)		; plain-text `Data Characters'
	(cond 
		((eq? html:blank txt) "&nbsp;")
		(else
			(if (symbol? txt) (set! txt (symbol->string txt)))
			(if (number? txt)
				(number->string txt)
				(irregex-replace
					">"
					(irregex-replace
							"<"
							(irregex-replace 
								"&" 
								txt
								"&amp;")
							"&lt;")
					"&gt;")
				))))

;;@args title backlink tags ...
;;@args title backlink
;;@args title
;;
;;Returns header string for an HTML page named @1.  If @2 is a string,
;;it is used verbatim between the @samp{H1} tags; otherwise @1 is
;;used.  If string arguments @3 ... are supplied, then they are
;;included verbatim within the @t{<HEAD>} section.
(define (html:head title . args)
  (define backlink (if (null? args) #f (car args)))
  (if (not (null? args)) (set! args (cdr args)))
  (string-append
  	(format "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">\n")
    (format "<HTML>\n")
    (format " <HEAD>\n  <TITLE>~a</TITLE>\n  ~a\n </HEAD>\n"
	    (html:plain title) (apply string-append args))
   	(if backlink
			backlink
			(format "<BODY><H1>~a</H1>\n" (html:plain title)))))

;;@body Returns HTML string to end a page.
(define (html:body . body)
  (apply string-append
	  (append body (list (format "</BODY>\n</HTML>\n")))))

;;@args
;;@args port
;;Returns a string of the characters up to, but not including a
;;newline or end of file, updating @var{port} to point to the
;;character following the newline.  If no characters are available, an
;;end of file object is returned.  The @var{port} argument may be
;;omitted, in which case it defaults to the value returned by
;;@code{current-input-port}.
(define (read-line . port)
  (let* ((char (apply read-char port)))
		(if (eof-object? char)
			char
			(do ((char char (apply read-char port))
				 (clist '() (cons char clist)))
				((or (eof-object? char) (char=? #\newline char))
					(clean-line (list->string (reverse clist))))))))

(define (request-split line)
  (define lst '())
  (define len (string-length line))
  (let loop ([beg 0] [end 0])
		(cond
			((> end len)
				(reverse lst))
			((or (= end len) (char=? #\space (string-ref line end)))
				(set! lst (cons (substring line beg end) lst))
				(loop (+ end 1) (+ end 1)))
			(else (loop beg (+ end 1)))
		)
  )
)

(define string-ci->symbol
  (let ((s2cis (if (equal? "x" (symbol->string 'x))
		   string-downcase string-upcase)))
		(lambda (str) (string->symbol (s2cis str)))))

(define (string-index str chr)
	(define len (string-length str))
	(do ((pos 0 (+ 1 pos)))
			((or (>= pos len) (char=? chr (string-ref str pos)))
				(and (< pos len) pos))))

(define (clean-line str)
	(define idx (- (string-length str) 1))
	(if (char=? (string-ref str idx) #\return)
		(substring str 0 idx)
		str))
)
