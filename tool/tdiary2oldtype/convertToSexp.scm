#!/usr/local/bin/gosh

(use srfi-1)


(define (load-td2 port mode)
  (let ((state 'title )
        (str-list (port->string-list port)))
    (filter-map
     (lambda (str)
       (cond
        ((#/^TDIARY+/ str)
         #f)
        ((#/^[a-zA-Z-]+: / str)
         (string-append
          (if (#/^Date: / str)
              "("
              "")
          (let1 pair (string-split str #/[ ]+/)
                (format "(~a . ~a)" (car pair) (cadr pair)))))
        ((= 0 (string-length str))
         (set! state 'title)
         #f)
        ((#/^.$/ str)
         (set! state 'title)
         ")")
        (else
         (let1 str (regexp-replace-all #/[\"]/ str "'")
               (if (eq? 'c mode)
                   (format "(body . \"~a\")" str))
                   (case state
                     ((title)
                      (set! state 'body)
                      (format "(title . \"~a\")" str))
                     ((body)
                      (format "(body . \"~a\")" str)))))))
     str-list)))


(define (main argv)
  (print "(")
  (for-each print
            (load-td2
             (current-input-port)
             (string->symbol #?=(cadr argv))))
  (print ")"))



  

