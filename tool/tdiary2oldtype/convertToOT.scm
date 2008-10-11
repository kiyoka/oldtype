#!/usr/local/bin/gosh

(use srfi-1)
(use util.list)
(use rfc.uri)

;;
;; "20081231" => "2008_12_31"
;;
(define (convert-date date)
  (let ((y (substring date 0 4))
        (m (substring date 4 6))
        (d (substring date 6 8)))
    (string-append y "_" m "_" d)))


;;
;; "<%=a 'link|str' %>          =>  [[link|str]]
;; "<%=isbn_image 'id' %>       =>  ##(amazon id)
;;
(define (convert-command str)
  (let* ((str
          (regexp-replace-all #/<a[ ]+href='([^']+)'>([^<]+)<\/a>/ str
                              (lambda (m)
                                (string-append "[["
                                               (rxmatch-substring m 1)
                                               "|"
                                               (rxmatch-substring m 2)
                                               "]]"))))
         (str
          (regexp-replace-all #/<%=[ ]?a[ ]+'([^|]+)[|]([^']+)'[ ]+%>/ str
                              (lambda (m)
                                (string-append "[["
                                               (rxmatch-substring m 2)
                                               "|"
                                               (rxmatch-substring m 1)
                                               "]]"))))
         (str
          (regexp-replace-all #/<%=[ ]?isbn_image[ ]+'([^']+)'[ ]+%>/ str
                              (lambda (m)
                                (string-append "##(amazon "
                                               (rxmatch-substring m 1)
                                               ")")))))
    str))

  
(define (output-oldtype-file username date entry-data)

  (define (display-diary entry port)
    (for-each
     (lambda (lst)
       (when (string? (car lst))
         (begin
           (display (convert-command (car lst)) port)
           (newline port)
           (newline port))))
     entry))

  (define (display-comment entry port)
    (let1 name (assq-ref entry 'Name:)
          (for-each
           (lambda (x)
             (let1 str (car x)
                   (when (and (string? str)
                              (not (#/^TrackBack$/ name))
                              (not (#/porno/ str))
                              (not (#/casino/ str))
                              (not (#/viagra/ str)))
                     (begin
                       (display  (format "##(comment-data ~a ~a)"
                                         (uri-encode-string name)
                                         (uri-encode-string str))
                                 port)
                       (newline port)))))
           entry)))

  (call-with-output-file (format "./out/~a.~a.ot" username (convert-date date))
    (lambda (port)
      port
      (begin
        (for-each
         (lambda (entry) ;; entry is a diary or a comment.
           (cond
            ((assq-ref entry 'Title:)
             ;; diary
             (display-diary entry port))
            ;; comment
            (else
             (display-comment entry port))))
         entry-data)
        (display "##(comment)" port)
        (newline port)))))



(define (save-oldtype-data username diary-data comment-data)
  (let1 h (make-hash-table 'string=?)
        ;; make hash data ( key is string of date , value is alist of diary or comment)
        (for-each
         (lambda (pair)
           (hash-table-push! h (car pair) (cdr pair)))
         (append
          diary-data
          comment-data
          ))

        (hash-table-for-each
         h
         (lambda (k v)
           (output-oldtype-file username k (reverse v))))))


(define (main argv)
  (let ((username       (cadr argv))         
        (diary-file     (caddr argv))
        (comment-file   (cadddr argv)))
    (let ((diary-data
           (call-with-input-file diary-file read))
          (comment-data
           (call-with-input-file comment-file read)))
      (save-oldtype-data
       username
       diary-data
       comment-data))))
      
