;;;
;;; oldtype/core.scm - oldtype core api for kahua apps and batch programs.
;;;
;;;  Copyright (c) 2003-2006 Kiyoka Nishiyama, All rights reserved.
;;;
;;;  Permission is hereby granted, free of charge, to any person
;;;  obtaining a copy of this software and associated documentation
;;;  files (the "Software"), to deal in the Software without restriction,
;;;  including without limitation the rights to use, copy, modify,
;;;  merge, publish, distribute, sublicense, and/or sell copies of
;;;  the Software, and to permit persons to whom the Software is
;;;  furnished to do so, subject to the following conditions:
;;;
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;;
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;  AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
;;;  OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;;;  IN THE SOFTWARE.
;;;
;;; $Id: util.scm 148 2007-12-06 13:20:29Z kiyoka $
;;;
;;;

(define-module oldtype.core
  (use srfi-1)
  (use file.util)
  (use oldtype.log)
  (use oldtype.timeline)
  (use oldtype.page)
  (use oldtype.rss)
  (export 
   oldtype:load-page
   oldtype:pages->rss-string
   ))
(select-module oldtype.core)


(define (oldtype:load-page _site-root wikiname)
  (define (gen-sexp-filename1)
    (string-append _site-root "/tmp/oldtype/_out/" wikiname ".sexp"))
  (define (gen-sexp-filename2)
    (string-append "./" wikiname ".sexp"))
  (let1 filename (cond
                  ((file-exists? (gen-sexp-filename1))
                   (gen-sexp-filename1))
                  ((file-exists? (gen-sexp-filename2))
                   (gen-sexp-filename2))
                  (else
                   #f))
        (if filename
            (with-input-from-file filename
              (lambda ()
                (deserialize
                 (make <oldtype-page>)
                 (read (current-input-port)))))
            #f)))


;;
;; args:
;;   ( <oldtype-page> <oldtype-page> <oldtype-page> ... )
;; return:
;;   rss content string
;:
(define (oldtype:pages->rss-string base-url top-page pages)
  (let ((header
         `((url   . ,(string-append base-url (name-of top-page)))
           (title . ,(append (car (get-text-list top-page))))
           (desc  . ,(append (car (get-text-list top-page))))))
        (entries
         (map
          (lambda (p)
            (let1 latest-log (get-latest-log (timeline-of p))
                  `((url   . ,(string-append base-url (name-of p)))
                    (utc   . ,(utc-of latest-log))
                    (title . ,(append (car (get-text-list p)))))))
          pages)))
    (rss-format header entries)))


(provide "oldtype/core")
