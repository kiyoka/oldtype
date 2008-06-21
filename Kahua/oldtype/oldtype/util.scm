;;;
;;; oldtype/util.scm - util for OldType
;;;
;;;  Copyright (c) 2008 Kiyoka Nishiyama, All rights reserved.
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
;;; $Id: util.scm 199 2008-01-13 11:16:43Z kiyoka $
;;;

(define-module oldtype.util
  (use srfi-1)
  (use srfi-13)
  (use srfi-19)
  (use sxml.ssax)
  (use sxml.sxpath)
  (use text.parse)
  (use file.util)
  (use util.list)
  (use oldtype.pasttime)
  (export oldtype:otpath->wikiname
          oldtype:otpath->basename
          oldtype:version
          oldtype:site-url
          oldtype:rank-limit
          oldtype:get-string-of-today
          oldtype:editpath
          oldtype:user-local
          oldtype:user-backend
          oldtype:get-pagelist
          oldtype:parse-svninfo
          oldtype:date-string->date-alist
          oldtype:utc->date-string
          oldtype:utc->RFC822-date-string
          oldtype:utc->ago-string
          oldtype:grouping-blog-entries
          pretty-print-sexp))
(select-module oldtype.util)

(load "oldtype/version.kahua")


;;=================================================
;; Const values for OldType
;;
(define oldtype:version  *oldtype-version*)
(define oldtype:site-url "http://sourceforge.jp/projects/oldtype/")
(define oldtype:rank-limit 5)

;;=================================================
;; Utility for OldType
;;
(define (oldtype:otpath->basename filepath)
  (receive (path basename suffix)
      (decompose-path filepath)
    basename))

(define (oldtype:otpath->wikiname filepath)
  (oldtype:otpath->basename filepath))

(define (oldtype:get-string-of-today)
  (date->string (current-date) "~Y_~m_~d"))

(define (oldtype:editpath)
  (string-append
   (sys-getenv "OT_SITE")
   "/tmp/oldtype/edit"))

(define (oldtype:user-local)
  (sys-getenv "OT_USER_LOCAL"))

(define (oldtype:user-backend)
  (sys-getenv "OT_USER_BACKEND"))


;;=================================================
;; for Batch process
;;

(define (oldtype:get-pagelist regexp-string)
  (reverse
   (directory-list (oldtype:editpath)
                   :filter (lambda (filename)
                             (if regexp-string
                                 ((string->regexp regexp-string) filename)
                                 #t)))))

;;=================================================
;; for date string
;;
;;
;; str:
;;   "2007-09-25T12:54:09.955196Z"
;; result:
;;   ((nanosecond . 0) (second . 9) (minute . 54) (hour . 12) (day . 25) (month . 9) (year . 2007) (zone-offset . 0) (utc . 1190724849))
;;   
(define (oldtype:date-string->date-alist str)
  (let* ((splitted (map string->number (string-split str #/[TZ.\-:]/)))
         (date-object
          (make-date 0;; nanosecond
                     (sixth  splitted) ;;second
                     (fifth  splitted) ;;minute
                     (fourth splitted) ;;hour
                     (third  splitted) ;;day
                     (second splitted) ;;month
                     (first  splitted) ;;year
                     0 ;;zone-offset
                     )))
    `(
      (nanosecond  . ,(date-nanosecond date-object))
      (second      . ,(date-second date-object))
      (minute      . ,(date-minute date-object))
      (hour        . ,(date-hour date-object))
      (day         . ,(date-day date-object))
      (month       . ,(date-month date-object))
      (year        . ,(date-year date-object))
      (zone-offset . ,(date-zone-offset date-object))
      (utc         . ,(time->seconds (date->time-utc date-object))))))


;; 
;; result format:
;;  (
;;   ( ;; file1
;;    (name . "OldType")
;;    (commit_revision . "100")
;;    (commit_auther   . "user01")
;;    (date . "2007-09-24T14:18:01.076277Z")
;;       .
;;       .
;;       .
;;   )
;;   ( ;; file2
;;    (name . "OldType2")
;;    (commit_revision . "120")
;;    (commit_auther   . "user02")
;;    (date . "2007-09-24T14:18:01.076277Z")
;;   )
;;  )
(define (oldtype:parse-svninfo port)
  (define (date-string-as-current-locale str)
    (let* ((date-str (string-append (car (string-split str ".")) " +0000"))
           (utc      (date->time-utc (string->date date-str "~Y-~m-~dT~H:~M:~S ~z"))))
      (date->string (time-utc->date utc)
                    "~Y-~m-~d ~I:~M ~p (~z)")))
  (define (utc-as-current-locale str)
    (number->string
     (time->seconds
      (date->time-utc
       (string->date 
        (string-append (car (string-split str ".")) " +0000")
        "~Y-~m-~dT~H:~M:~S ~z")))))
  (let1 sxml
        (ssax:xml->sxml port '())
        (map
         (lambda (x)
           `(
             (name               . ,(oldtype:otpath->wikiname (second x)))
             (kind               . ,(first x))
             (path               . ,(second x))
             (revision           . ,(third x))
             (commit_revision    . ,(fourth x))
             (commit_author      . ,(fifth x))
             (commit_date        . ,(date-string-as-current-locale 
                                     (sixth x)))
             (commit_utc         . ,(utc-as-current-locale 
                                     (sixth x)))
             ))
         (zip
          ((sxpath "//info/entry/@kind/text()") sxml)
          ((sxpath "//info/entry/@path/text()") sxml)
          ((sxpath "//info/entry/@revision/text()") sxml)
          ((sxpath "//info/entry/commit/@revision/text()") sxml)
          ((sxpath "//info/entry/commit/author/text()") sxml)
          ((sxpath "//info/entry/commit/date/text()") sxml)
          ))))

;;
;; imported from this URL ( written by bizen )
;;    http://practical-scheme.net/wiliki/wiliki.cgi?Gauche%3APrettyPrint
;; 
(define (pretty-print-sexp s)
  (define (do-indent level)
    (dotimes (_ level) (write-char #\space)))
  (define (pp-parenl)
    (write-char #\())
  (define (pp-parenr)
    (write-char #\)))
  (define (pp-atom e prefix)
    (when prefix (write-char #\space))
    (write e))
  (define (pp-list s level prefix)
    (and prefix (do-indent level))
    (pp-parenl)
    (let loop ((s s)
               (prefix #f))
      (if (null? s)
          (pp-parenr)
          (let1 e (car s)
            (if (list? e)
                (begin (and prefix (newline))
                       (pp-list e (+ level 1) prefix))
                (pp-atom e prefix))
            (loop (cdr s) #t)))))
  (if (list? s)
      (pp-list s 0 #f)
      (write s))
  (newline))


;;
;; Convert utc seconds to "2008-03-20 09:36 PM (+0900)"
;;
(define (oldtype:utc->date-string utc)
  (if utc
      (let1 d (time-utc->date
               (seconds->time
                utc))
            (string-append (date->string d "~Y-~m-~d ~H:~M (~z)")))
      "*NoDateInformation*"))

;;
;; Convert utc seconds to RFC822 like "20 Mar 2008 18:36:00 +0000"
;;
(define (oldtype:utc->RFC822-date-string utc)
  (sys-strftime "%d %b %Y %H:%M:%S +0000" (sys-gmtime utc)))

;;
;; Convert utc seconds to "    (10 seconds ago)"
;;
(define (oldtype:utc->ago-string utc)
  (if utc
      (string-append
       (format "~16,,,' @a"
               (string-append
                "("
                (how-long-since utc)
                " ago)")))
      "*NoDateInformation*"))

;;
;; grouping blog entry list by month
;;
;; arg:
;;   ("kiyoka.2008_10_01.ot" "kiyoka.2008_10_03.ot" ...)
;; 
;; result:
;;   (
;;     (YEAR_MONTH LIST-OF-ENTRY)
;;     (2008_10 ("kiyoka.2008_10_01.ot" "kiyoka.2008_10_03.ot" ...))
;;     (2008_11 ("kiyoka.2008_11_02.ot" "kiyoka.2008_11_03.ot" ...))
;;   )
;;
(define (oldtype:grouping-blog-entries entrylist)
  (define (check-format str)
    (#/^[^.]+[.][0-9]+_[0-9]+_[0-9]+/ str))

  (let ((valid-entries
         (reverse
          (sort
           (filter
            (lambda (name)
              (check-format name))
            entrylist))))
        (ht (make-hash-table 'string=?)))
    (for-each
     (lambda (name)
       (let* ((lst (string-split name #/[._]/))
              (str (string-append (second lst)  ;; year
                                  "_"
                                  (third  lst)) ;; month
                   ))
         (hash-table-push! ht
                           str
                           name)))
     valid-entries)
    (hash-table->alist ht)))


(provide "oldtype/util")
