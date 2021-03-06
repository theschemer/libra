(library (libra libra)
    (export
        get!
        post!
        router!
        default-make-response
        default-make-json 
        view
        params-ref
        libra:run)
    (import
        (scheme)
        (except (surfage s13 strings)
            string-for-each string-fill! string-copy 
            string->list string-copy! string-titlecase 
            string-upcase string-downcase string-hash)
        (libc libc)
        (irregex irregex)
        (json json)
        (libra mime)
        (libra server http-cgi)
        (libra server tiny))

    ;; 路由哈希表
    (define router-handler (make-hashtable string-hash string=?))

    ;; 配置字典
    (define libra-options (make-hashtable string-hash string=?))

    ;; 定义http方法函数
    (define (get! url handler) (router! "get" url handler))
    (define (post! url handler) (router! "post" url handler))
    ;; (define (put! url handler) (route! 'put url handler))
    ;; (define (delete! url handler) (route! 'delete url handler))

    ;; http response maker
    (define (default-make-response html)
        (http:content
            '(("Content-Type" . "text/html") ("Connection" . "close"))
                html))

    ;; 定义路由函数
    (define (router! method url handler)
        ;; url 转化成 regex和key-list
        (define keys/reg (cons #f url))
        (if (number? (string-index url #\:))
            (set! keys/reg (url->keys/reg url)))
        (hashtable-set! router-handler (string-append method " " (cdr keys/reg)) (cons (car keys/reg) handler))
    )

    ;; 从 hashtable 中获取对应keys/handlder
    (define (request->keys/handler request-line query-string)
        (let  [(method (car request-line)) (router (url->router (cadr request-line)))]
            (if (eq? #f router)
                (cons #f #f)
                (router->keys/handler (string-append (symbol->string method) " " router) query-string)))
    )  

    ;;从request中找到纯净的路由
    (define (url->router url)
        (if (string? url)
            (if (eq? #f (string-index url #\?))
                url
                (substring url 0 (string-index url #\?)))
            #f))

    ;; 分析url, 获取路由正则和参数名称list
    (define (url->keys/reg url)
        (libra-regex-path url))

    ;; 遍历路由表，获取对应的路由的keys/handler, 默认 '(#f . #f)
    (define (router->keys/handler router query)
        (define router-vector (hashtable-keys router-handler))
        (define router-length (vector-length router-vector))
        (define keys/handler (cons #f #f))
        (define keys (if query (libra-regex-query query) '()))
        (let loop ((index 0))
            (begin
                (let [(reg-router (vector-ref router-vector index))]
                    ; (printf "~a\n" reg-router)
                    (if (string-index reg-router #\?)
                        (begin
                            (let ((values (libra-regex-router reg-router router)))
                                (if (not (null? values))
                                    (begin
                                        (let ((names/handler (hashtable-ref router-handler reg-router '(#f . #f))))
                                            (map (lambda (name value) (set! keys (cons (cons name value) keys))) (car names/handler) values)
                                            (set! keys/handler (cons keys (cdr names/handler)))
                                        )
                                    )
                                )
                            )
                        )
                        (if (equal? router reg-router)
                            (set! keys/handler (cons keys (cdr (hashtable-ref router-handler router '(#f . #f))))))))
                (if (and (< index (- router-length 1)) (equal? keys/handler '(#f . #f)))
                    (loop (+ 1 index))
                )
            )
        )
        keys/handler
    )

    ;; 匹配路由和动态里路，返回参数列表
    (define (libra-regex-router reg str)
        (define match (irregex-search reg str))
        (define result '())
        (when match
            (let loop ([idx 8])
                (when (vector-ref match idx)
                    (set! result (cons (substring str (vector-ref match idx) (vector-ref match (+ idx 2))) result))
                    (loop (+ idx 4))
                )
            )
        )
        (reverse result) 
    )

    ;; 获取动态路由的参数列表和正则表示
    (define (libra-regex-path str)
        (define pattern "/:([^\\/]+)")
        (define replace "/([^/?]+)")
        (define len (string-length str))
        (define result '())
        (let loop ([match (irregex-search pattern str 0 len)])
            (when match
                (set! result (cons (substring str (vector-ref match 8) (vector-ref match 10)) result))  
                (loop (irregex-search pattern str (vector-ref match 10) len))
            )
        )
        (cons 
            (reverse result) 
            (irregex-replace/all pattern str replace)
        )
    )

    ;; 获取url参数关联表
    (define (libra-regex-query query)
        (define pattern "([^\\/=&]+)=([^=&]*)")
        (define len (string-length query))
        (define keys '())
        (let loop ([match (irregex-search pattern query 0 len)])
            (when match
                (set! keys (cons (cons 
                                    (substring query (vector-ref match 8) (vector-ref match 10))
                                    (substring query (vector-ref match 12) (vector-ref match 14)))
                                keys))
                (loop (irregex-search pattern query (vector-ref match 14) len))
            )
        )
        (reverse keys)
    )

    ;; 读取文本文件
    (define read-file
        (lambda (file-name)
            (let ((p (open-input-file file-name)))
                (let loop ((lst '()) (c (read-char p)))
                    (if (eof-object? c)
                        (begin 
                            (close-input-port p)
                            (list->string (reverse lst)))
                        (loop (cons c lst) (read-char p)))))))

    ;; 返回视图函数
    (define view
        (lambda (file-name)
            (unless (string-index file-name #\.)
                (set! file-name (string-append 
                                    (hashtable-ref libra-options "web-path" (get-app-path))
                                    "/"
                                    (hashtable-ref libra-options "view-path" "views")
                                    "/"
                                    file-name
                                    ".html")))
            (default-make-response (read-file file-name))))

    ;; 默认json返回
    (define (default-make-json data)
        (http:content
            '(("Content-Type" . "application/json; charset=utf-8") ("Connection" . "close"))
                (scm->json-string data)))

    ;; 判断资源文件
    (define (resource? request)
        (not (not (string-index request #\.))))

    ;; 返回资源对应http头
    (define (get-content-type type)
        (cons "Content-Type" (symbol->string (get-mime-type type 'text/html)))
    )

    ;; 返回资源文件
    (define (default-make-resource request)
        (let* ([file-path (get-file-path request)]
               [exist? (file-exists? file-path)])
            (if exist?
                (let* ([flen (get-file-length file-path)]
                       [line (string-append
                                (http:status-line 200 "OK")
                                (http:header
                                    (list
                                        (get-content-type (substring request (+ 1 (string-index-right request #\.)) (string-length request)))
                                        (cons "Content-Length" (number->string flen))
                                        (cons "Connection" "close"))))]
                       [lbv (string->utf8 line)]
                       [llen (bytevector-length lbv)]
                       [fbuf (make-bytevector flen)]
                       [f (c-fopen file-path "r")]
                       [fr (c-fread fbuf flen 1 f)]
                       [buf (make-bytevector (+ llen flen))])
                    (bytevector-copy! lbv 0 buf 0 llen)
                    (bytevector-copy! fbuf 0 buf llen flen)
                    (c-fclose f)
                    buf
                ) 
                '(404 "Bad Request")
            )
        )
    )

    ;; 获取文件长度
    (define (get-file-length file-path)
        (let* ([p (open-input-file file-path)]
               [len (file-length p)])
            (close-port p)
            len
        )
    )

    ;; 获取执行文件文件夹地址
    (define (get-app-path)
        (define script (car (command-line)))
        (define index-right (string-index-right script #\\))
        (define index-left (string-index-right script #\/))
        (define path (substring script 0 (max (if (number? index-right) index-right 0) (if (number? index-left) index-left 0))))
        (if (string=? "" path)
            "."
            path
        )
    )

    ;; 展示字典
    (define (hashtable-show ht)
        (vector-map 
            (lambda (k) (printf "~a : ~a\n" k (hashtable-ref ht k ""))) 
            (hashtable-keys ht))
    )

    ;; 获取web配置
    (define (get-option key . rest)
        (hashtable-ref libra-options key (if (null? rest) #f (car rest))))

    ;; 设置配置
    (define (set-opiton! key value)
        (hashtable-set! libra-options key value))

    ;; 获取文件完整路径
    (define (get-file-path file)
        (string-append (hashtable-ref libra-options "web-path" (get-app-path)) file))

    ;; 文件导入
    (define (using file)
        (load (string-append (hashtable-ref libra-options "app-path" (get-app-path)) (string (directory-separator)) file)))

    ;; 路由函数参数解析
    (define (params-ref p key . default)
        (let ((pair (assoc key p)))
            (if pair
                (cdr pair)
                (if (null? default) #f (car default)))
        )
    )

    ;; 初始化操作
    (define libra-init
        (begin
            ;; web根目录
            (hashtable-set! libra-options "web-path" (get-app-path))
            ;; 视图文件夹名称
            (hashtable-set! libra-options "view-path" "views")
            ;; 启动文件目录
            (hashtable-set! libra-options "app-path" (get-app-path))
            0
        )    
    )

    ;; 默认服务器处理 入口
    (define libra-proc
        (lambda (request-line query-string header)
            ;; show msg on server
            (printf "HTTP=~a\n" request-line)
            (printf "path=~a\n" (cadr request-line))
            (if (resource? (cadr request-line))
                (default-make-resource (cadr request-line))
                (let [(keys/handler (request->keys/handler request-line query-string))]
                    (if (procedure? (cdr keys/handler))
                        ((cdr keys/handler) (car keys/handler))
                        '(404 "Bad Request"))))))
    
    (define libra:run 
        (case-lambda
            ([port]
                (libra:run port "127.0.0.1"))
            ([port ip]
                (libra:run libra-proc port ip))
            ([proc port ip]
                (libra:run tiny:run proc port ip))
            ([server proc port ip]
                (server proc port ip))))
)
