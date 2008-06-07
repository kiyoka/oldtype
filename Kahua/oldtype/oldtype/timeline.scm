;;;
;;; oldtype/timeline - manage log and annotate information of single wiki page.
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
;; timeline information of a single wiki page.
;;

(define-module oldtype.timeline
  (use srfi-1)
  (use srfi-13)
  (use sxml.ssax)
  (use sxml.sxpath)
  (use util.list)
  (use oldtype.util)
  (use oldtype.log)
  (use gauche.sequence)
  (export <oldtype-timeline>
          annotation-of
          parse
          serialize
          deserialize
          log-by-lineno
          rank-by-lineno
          text-by-lineno
          log-by-revision
          get-latest-log
          text-of
          ))
(select-module oldtype.timeline)


(define-class <oldtype-timeline> ()
  (;; Customization parameters -----------------------
   ;; page name (utf-8)
   (name        :accessor name-of        :init-keyword :name
                :init-value "none")
   ;; latest revision no
   (revision    :accessor revision-of    :init-keyword :revision
                :init-value 0)
   ;; (REVISION . <oldtype-log>) alist in page ( sorted by REVISION order by newer )
   (log         :accessor log-of         :init-keyword :log
                :init-value '())
   ;; vector of <oldtype-log> in page
   (annotation  :accessor annotation-of  :init-keyword :annotation
                :init-value '())
   ;; vector of text in page
   (text        :accessor text-of        :init-keyword :text
                :init-value '())
   ))

;;=================================================
;; parse svn log
;;
;;
;; result format:
;;   (
;;    ((revision . 154) (committer . kiyoka) (date . date-alist) (date-str . "2007-09-25T12:54:09.955196Z"))
;;    ((revision . 153) (committer . kiyoka) (date . date-alist) (date-str . "2007-09-25T12:19:35.838490Z"))
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
           (let1 date-alist (oldtype:date-string->date-alist (third x))
                 `(
                   (revision  . ,(string->number (first x)))
                   (committer . ,(string->symbol (second x)))
                   (date      . ,date-alist)
                   (utc       . ,(assq-ref date-alist 'utc))
                   (date-str  . ,(third x)))))
         (zip
          ((sxpath "//log/logentry/@revision/text()") sxml)
          ((sxpath "//log/logentry/author/text()") sxml)
          ((sxpath "//log/logentry/date/text()") sxml)))))

;; 
;; result format:
;;  (
;;   (
;;     
;;    ((revision . 140) (committer . kiyoka) (text . "* What is OldType"))
;;    ((revision . 142) (committer . kiyoka) (text . "- [[OldType]]"))
;;    ((revision . 143) (committer . kiyoka) (text . "- OldType development blog is [[@username:blog]]"))
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
                 (rev        (string-trim-both (read-string 6)))
                 (committer  (string-trim-both (read-string 12)))
                 (text       (next-token '() '(#\newline *eof*)))
                 (_          (read-char)))
             (loop `((
                      (revision  . ,(string->number rev))
                      (committer . ,(string->symbol committer))
                      (text      . ,text))
                     ,@lst))))))))))


;;
;; access <oldtype-log> by revision no
;;
(define-method log-by-revision ((self <oldtype-timeline>) rev)
  (assq-ref (log-of self) rev))

(define-method get-latest-log ((self <oldtype-timeline>))
  (log-by-revision self (revision-of self)))

;;
;; result:
;;  revision numbers in page.
;; example:
;;     revision . rank
;;   '((420     . 0)
;;     (313     . 1)
;;     (234     . 2)
;;     (51      . 3)
;;     (20      . 4))
(define (calc-revision-alist log ann)
  ;; like...
  ;;   '(420 
  ;;     313 
  ;;     234 
  ;;      51 
  ;;      20)
  (if (and log ann)
      (let* ((revision-list
              (reverse (sort
                        (delete-duplicates (map
                                            (lambda (alist)
                                              (assq-ref alist 'revision))
                                            ann)))))
             ;; ( 532 234 ) will be padding zeros ( 0 0 0 0 532 234 )
             (revision-list-more5-element
              (if (<= (+ 1 oldtype:rank-limit) (length revision-list))
                  revision-list
                  (append
                   (make-list (- (+ 1 oldtype:rank-limit) (length revision-list)) 0)
                   revision-list))))
        (map-with-index
         (lambda (index x)
           (cons x index))
         revision-list-more5-element))
      '()))


;;
;; parse log-file and ann-file to <oldtype-timeline> object
;;
(define-method parse ((self <oldtype-timeline>) log-file ann-file)
  (let* ((log (oldtype:parse-log log-file))
         (ann (oldtype:parse-annotate ann-file))
         (revision-alist (calc-revision-alist log ann)))

    (set! (log-of self)
          (sort
           (map
            (lambda (a-list)
              (let1 rev (assq-ref a-list 'revision)
                    (cons
                     rev
                     (make <oldtype-log>
                       :revision rev
                       :committer (assq-ref a-list 'committer)
                       :utc (assq-ref a-list 'utc)
                       :rank (if (assq-ref revision-alist rev)
                                 (assq-ref revision-alist rev)
                                 oldtype:rank-limit)))))
            log)
           (lambda (a b)
             (> (car a) (car b)))))

    ;; building annotation information
    (set! (annotation-of self)
          (list->vector
           (map
            (lambda (a-list)
              (let1 rev (assq-ref a-list 'revision)
                    (log-by-revision self rev)))
            ann)))

    ;; building text information
    (set! (text-of self)
          (list->vector
           (map
            (lambda (a-list)
              (assq-ref a-list 'text))
            ann)))

    ;; latest revision no
    (set! (revision-of self) 
          (first
           (reverse
            (sort
             (map
              car
              revision-alist)))))
    
    self))


(define-method serialize ((self <oldtype-timeline>))
  `((name       . ,(name-of self))
    (revision   . ,(revision-of self))
    (log        . ,(map
                    (lambda (x)
                      (cons
                       (car x)
                       (serialize (cdr x))))
                    (log-of self)))
    (annotation . ,(map
                    serialize
                    (vector->list (annotation-of self))))
    (text       . ,(vector->list (text-of self)))))



;;
;; serialize <oldtype-timeline>
;;
(define-method deserialize ((dummy <oldtype-timeline>) sexp)
  (make <oldtype-timeline>
    :name       (assq-ref sexp 'name)
    :revision   (assq-ref sexp 'revision)
    :log        (map
                 (lambda (x)
                   (cons
                    (car x)
                    (deserialize (make <oldtype-log>) (cdr x))))
                 (assq-ref sexp 'log))
    :annotation (list->vector
                 (map
                  (lambda (x)
                    (deserialize (make <oldtype-log>) x))
                  (assq-ref sexp 'annotation)))
    :text       (list->vector
                 (assq-ref sexp 'text))))


;; access <oldtype-log> by lineno
;;
(define-method log-by-lineno ((self <oldtype-timeline>) lineno)
  (ref (annotation-of self) (- lineno 1) #f))


;; latest ranking by lineno
;;
(define-method rank-by-lineno ((self <oldtype-timeline>) lineno)
  (let1 log (log-by-lineno self lineno)
        (if log
            (rank-of log)
            0)))


;; access <oldtype-log> by lineno
;;
(define-method text-by-lineno ((self <oldtype-timeline>) lineno)
  (ref (text-of self) (- lineno 1) #f))


(provide "oldtype/timeline")
