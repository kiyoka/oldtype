;;;
;;; oldtype/log - manage single svn log information.
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
;;;  $Id: $
;;;
;;; 

;;
;; svn log information.
;;

(define-module oldtype.log
  (use srfi-1)
  (use srfi-13)
  (use util.list)
  (use oldtype.util)
  (export <oldtype-log>
          parse
          serialize
          deserialize
          get-ago
          get-date
          rank-of
          committer-of
          revision-of
          ))
(select-module oldtype.log)


(define-class <oldtype-log> ()
  (;; Customization parameters -----------------------
   ;; revision
   (revision    :accessor revision-of    :init-keyword :revision
                :init-value 1)
   ;; committer
   (committer   :accessor committer-of   :init-keyword :committer
                :init-value "none")
   ;; utc
   (utc         :accessor utc-of         :init-keyword :utc
                :init-value 0)
   ;; ranking value 0 to N ( 0 is the top value )
   (rank        :accessor rank-of        :init-keyword :rank
                :init-value 0)
   ))


(define-method serialize ((self <oldtype-log>))
  `((revision   . ,(revision-of self))
    (committer  . ,(committer-of self))
    (utc        . ,(utc-of self))
    (rank       . ,(rank-of self))))

;;
;; deserialize <oldtype-log>
;;
(define-method deserialize ((dummy <oldtype-log>) sexp)
  (make <oldtype-log>
    :revision       (assq-ref sexp 'revision)
    :committer      (assq-ref sexp 'committer)
    :utc            (assq-ref sexp 'utc)
    :rank           (assq-ref sexp 'rank)))

(define-method get-ago ((self <oldtype-log>))
  (oldtype:utc->ago-string (utc-of self)))

(define-method get-date ((self <oldtype-log>))
  (oldtype:utc->date-string (utc-of self)))

(provide "oldtype/log")
