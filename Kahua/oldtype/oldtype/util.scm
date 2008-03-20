;;;
;;; oldtype/format.scm - format wiki pages (backward compatibility module)
;;;
;;;  Copyright (c) 2003-2006 Shiro Kawai, All rights reserved.
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
;;; Modified by kiyoka to implement OldType wiki formatter.
;;; I renamed namespace of wiliki- 'oldtype-' to avoid collision of
;;; installation.
;;;

(define-module oldtype.util
  (use srfi-1)
  (use srfi-13)
  (use srfi-19)
  (use sxml.ssax)
  (use sxml.sxpath)
  (use text.parse)
  (use file.util)
  (export oldtype:otpath->wikiname
          oldtype:otpath->basename
          oldtype:version
          oldtype:site-url
          oldtype:otpath->wikiname
          oldtype:get-string-of-today
          oldtype:editpath
          oldtype:user-local
          oldtype:user-backend
          oldtype:get-pagelist
          oldtype:parse-log
          oldtype:parse-annotate
          oldtype:parse-svninfo))
(select-module oldtype.util)

(load "oldtype/version.kahua")


;;=================================================
;; Const values for OldType
;;
(define oldtype:version  *oldtype-version*)
(define oldtype:site-url "http://sourceforge.jp/projects/oldtype/")

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
   (sys-getenv "OT_EDITHOME")
   "/edit"))

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
;;   ((nanosecond . 0) (second . 9) (minute . 54) (hour . 12) (day . 25) (month . 9) (year . 2007) (zone-offset . 0))
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
      (zone-offset . ,(date-zone-offset date-object)))))

;;=================================================
;; parse svn log
;;

;;
;; result format:
;;   (
;;     revision  alist
;;    (154        ( (user . kiyoka) (date . <date>-value) (date-str . "2007-09-25T12:54:09.955196Z")))
;;    (153        ( (user . kiyoka) (date . <date>-value) (date-str . "2007-09-25T12:19:35.838490Z")))
;;                .
;;                .
;;   )
(define (oldtype:parse-log logfile)
  (let1 sxml
        (with-input-from-file logfile
          (lambda ()
            (ssax:xml->sxml (current-input-port) '())))
        
        (map
         (lambda (x)
           `(
             ,(string->number (first x))
             (
              (user     . ,(string->symbol (second x)))
              (date     . ,(oldtype:date-string->date-alist (third x)))
              (date-str . ,(third x)))))
         (zip
          ((sxpath "//log/logentry/@revision/text()") sxml)
          ((sxpath "//log/logentry/author/text()") sxml)
          ((sxpath "//log/logentry/date/text()") sxml)))))

;; 
;; result format:
;;  (
;;   (
;;     revision aliat
;;    (140      ((user . kiyoka) (str . "* What is OldType")))
;;    (142      ((user . kiyoka) (str . "- [[OldType]]")))
;;    (143      ((user . kiyoka) (str . "- OldType development blog is [[@username:blog]]")))
;;          .
;;          .
;;          .
;;   )
;;  )
(define (oldtype:parse-annotate annfile)
  (reverse
   (with-input-from-file annfile
     (lambda ()
       (let loop ((lst '()))
         (cond
          ((eof-object? (peek-char (current-input-port)))
           lst)
          (else
           (let (
                 (rev  (string-trim-both (read-string 6)))
                 (user (string-trim-both (read-string 12)))
                 (text (next-token '() '(#\newline *eof*)))
                 (_    (read-char)))
             (loop (cons `(
                           ,(string->number rev)
                           (
                            (user . ,(string->symbol user))
                            (str  . ,text)))
                         lst))))))))))

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
    (date->string (string->date str "~Y-~m-~dT~H:~M:~S")
                  "~1 ~3 (UTC)"))
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
             ))
         (zip
          ((sxpath "//info/entry/@kind/text()") sxml)
          ((sxpath "//info/entry/@path/text()") sxml)
          ((sxpath "//info/entry/@revision/text()") sxml)
          ((sxpath "//info/entry/commit/@revision/text()") sxml)
          ((sxpath "//info/entry/commit/author/text()") sxml)
          ((sxpath "//info/entry/commit/date/text()") sxml)
          ))))

(provide "oldtype/util")
