#!/usr/local/bin/gosh

(use srfi-1)
(use util.list)

;;
;; "20081231" => "2008_12_31"
;;
(define (convert-date date)
  (let ((y (substring date 0 4))
        (m (substring date 4 6))
        (d (substring date 6 8)))
    (string-append y "_" m "_" d)))

(define (output-oldtype-file date entry-data)
  (call-with-output-file (format "./out/xxxx.~a.ot" (convert-date date))
    (lambda (port)
      port
      (for-each
       (lambda (entry) ;; entry is a diary or a comment.
         (cond
          ;; diary
          ((assq-ref entry 'Title:)
           (for-each
            (lambda (lst)
              (when (string? (car lst))
                (begin
                  (display (car lst) port)
                  (newline port)
                  (newline port))))
            entry)
           (display "##(comment)" port)
           (newline port))))
       entry-data))))


;; comment


;;          (else
;;           #f)
;;   
;;           (write (assq-ref entry 'Name:))
;;           (for-each
;;            (lambda (lst)
;;              (if (string? (car lst))
;;                  (print (car lst))))))))))))


(define (save-oldtype-data diary-data comment-data)
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
           (output-oldtype-file k v)))))


(define (main argv)
  (let ((diary-file     (cadr argv))
        (comment-file   (caddr argv)))
    (let ((diary-data
           (call-with-input-file diary-file read))
          (comment-data
           (call-with-input-file comment-file read)))
      (save-oldtype-data
       diary-data
       comment-data))))
      
