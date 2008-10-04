#!/usr/local/bin/gosh

(use srfi-1)


(define (load-td2 port)
  (let (
        (body  '())
        (str-list (port->string-list port)))

    (define (reset-body!)
      (let1 ret (reverse body)
            (set! body '())
            (if (null? ret)
                ""
                (string-append "    ( \""
                               (string-join ret "\n")
                               "\" )"))))
    (define (push-body! str)
      (push! body str))

    (filter-map
     (lambda (str)
       (cond
        ((#/^TDIARY+/ str)
         #f)
        ((#/^[a-zA-Z-]+: / str)
         (let1 pair (string-split str #/[ ]+/)
               (string-append
                (if (#/^Date: / str)
                    (format "( \"~a\" . " (cadr pair))
                    "")
                (format "(~a . \"~a\")" (car pair) (cadr pair)))))
        ((= 0 (string-length str))
         (reset-body!))
        ((#/^.$/ str)
         (string-append (reset-body!)
                        " )"))
        (else
         (let1 str (regexp-replace-all #/[\"]/ str "'")
               (push-body! str)
               #f))))
     str-list)))


(define (main argv)
  (let1 mode (string->symbol (cadr argv))
        (print "(")
        (for-each print
                  (load-td2
                   (current-input-port)))
        (print ")")))


  

