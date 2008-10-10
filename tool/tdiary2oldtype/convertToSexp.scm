#!/usr/local/bin/gosh

(use srfi-1)
(use srfi-13)

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
      (if (not (and (null? body) (= 0 (string-length str))))
          (push! body str)))

    (filter-map
     (lambda (str)
       (cond
        ((#/^TDIARY+/ str)
         #f)
        ((#/^[a-zA-Z-]+: / str)
         (let1 pair (string-split str #/[ ]+/)
               (string-append
                (if (#/^Date: / str)
                    (begin
                      (reset-body!)
                      (format "( \"~a\" " (cadr pair)))
                    "")
                (format "(~a . \"~a\")" (car pair) (cadr pair)))))
        ((#/^[.]$/ str)
         (string-append (reset-body!)
                        " )"))
        (else
         (let1 str (string-trim
                    (regexp-replace-all #/[\"]/ str "'"))
               (push-body! str)
               #f))))
     str-list)))


(define (main argv)
  (print "(")
  (for-each print
            (load-td2
             (current-input-port)))
  (print ")"))


  

