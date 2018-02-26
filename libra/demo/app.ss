(import (libra libra))

(get! "/"
  (lambda (p)
  ; (view "./views/index.html")
  (view "index")))

(post! "/" 
  (lambda (p) (default-make-response "POST request")))
  
(get! "/blog/:user/:age" 
  (lambda (p)
    (default-make-json p)))

(libra:run 8011)







