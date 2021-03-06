;;;
;;; oldtype/page - wiki page information
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
;; page information of single wiki page of OldType.
;; it contains multi <oldtype-line> information
;;

(define-module oldtype.page
  (use srfi-1)
  (use srfi-13)
  (use util.list)
  (use oldtype.log)
  (use oldtype.timeline)
  (use oldtype.parse)
  (use oldtype.format)
  (use oldtype.util)
  (use gauche.sequence)
  (export <oldtype-page>
          serialize
          deserialize
          parse
          name-of
          sxml-of
          timeline-of
          get-revision
          get-ago
          get-date
          get-committer
          get-rank
          get-rank-list
          get-text
          get-text-list
          get-plain-list
          get-rich-list
          get-command-list
          get-rss-entry-pages
          ))
(select-module oldtype.page)


(define-class <oldtype-page> ()
  (;; Customization parameters -----------------------

   ;; page name (utf-8)
   (name        :accessor name-of        :init-keyword :name
                :init-value "none")
   ;; sxml data in page
   (sxml        :accessor sxml-of        :init-keyword :sxml
                :init-value '())
   ;; <oldtype-timeline> in page
   (timeline    :accessor timeline-of    :init-keyword :timeline
                :init-value '())
   ;; vector of text in page
   (plain       :accessor plain-of       :init-keyword :plain
                :init-value '())
   ;; vector of rich text in page
   (rich        :accessor rich-of        :init-keyword :rich
                :init-value '())
   ;; list of commands in page
   (commands    :accessor commands-of    :init-keyword :commands
                :init-value '())
   ))


;; Parsing wiki format
;;
;;   In case of log-file or ann-file is #f, this function takes only parse action.
;;
(define-method parse ((self <oldtype-page>) wiki-port log-file ann-file)
  (let1 timeline (make <oldtype-timeline> :name (name-of self))
        (set! (timeline-of self)
              (if (and log-file ann-file)
                  (parse timeline log-file ann-file)
                  timeline)))
  (set! (sxml-of self)
        (oldtype:sxml->internal
         (oldtype-parse wiki-port)))
  (set! (plain-of self)
        (list->vector
         (string-split
          (oldtype:sxml->plain-text (sxml-of self) #f)
          #\newline)))
  (set! (rich-of self)
        (list->vector
         (string-split
          (oldtype:sxml->plain-text (sxml-of self) #t)
          #\newline)))
  (set! (commands-of self)
        (oldtype:sxml->command-list (sxml-of self) (name-of self)))
  self)


(define-method serialize ((self <oldtype-page>))
  `(
    (name     . ,(name-of     self))
    (sxml     . ,(sxml-of     self))
    (timeline . ,(serialize (timeline-of self)))
    (plain    . ,(vector->list (plain-of self)))
    (rich     . ,(vector->list (rich-of self)))
    (commands . ,(commands-of self))))


(define-method deserialize ((dummy <oldtype-page>) internal-data)
  (make <oldtype-page>
    :name      (assq-ref internal-data 'name)
    :sxml      (assq-ref internal-data 'sxml)
    :timeline  (deserialize (make <oldtype-timeline>) (assq-ref internal-data 'timeline))
    :plain     (list->vector (assq-ref internal-data 'plain))
    :rich      (list->vector (assq-ref internal-data 'rich))
    :commands  (assq-ref internal-data 'commands)))


(define-method get-revision ((self <oldtype-page>) lineno)
  (revision-of (log-by-lineno (timeline-of self) lineno)))

(define-method get-ago ((self <oldtype-page>) lineno)
  (get-ago (log-by-lineno (timeline-of self) lineno)))

(define-method get-date ((self <oldtype-page>) lineno)
  (get-date (log-by-lineno (timeline-of self) lineno)))

(define-method get-committer ((self <oldtype-page>) lineno)
  (symbol->string (committer-of (log-by-lineno (timeline-of self) lineno))))

(define-method get-rank ((self <oldtype-page>) lineno)
  (rank-of (log-by-lineno (timeline-of self) lineno)))

(define-method get-rank-list ((self <oldtype-page>))
  (map
   (lambda (log)
     (rank-of log))
   (annotation-of (timeline-of self))))

(define-method get-text ((self <oldtype-page>) lineno)
  (text-by-lineno (timeline-of self) lineno))

(define-method get-text-list ((self <oldtype-page>))
  (vector->list (text-of (timeline-of self))))

(define-method get-plain-list ((self <oldtype-page>))
  (vector->list (plain-of self)))

(define-method get-rich-list ((self <oldtype-page>))
  (vector->list (rich-of self)))

(define-method get-command-list ((self <oldtype-page>))
  (commands-of self))


(define-method _get-rss-lineno-list ((self <oldtype-page>))
  (filter
   (lambda (x)
     x)
   (map-with-index
    (lambda (index str)
      (if (and
           (#/\[\[.+\]\]/ str)
           (or
            (#/^\*{1,3} / str)
            (#/^\-{1,3} / str)
            (#/^\#{1,3} / str)))
          (+ index 1)
          #f))
    (get-text-list self))))


(define-method get-rss-entry-pages ((self <oldtype-page>))
  (delete-duplicates
   (filter-map
    (lambda (lineno)
      (let* ((m (rxmatch #/\[\[([^\]|]+)\]\]/ (get-text self lineno)))
             (wikiname
              (if m
                  (rxmatch-substring m 1)
                  #f)))
        (if m
            (cons
             lineno
             wikiname)
            #f)))
    (_get-rss-lineno-list self))
   (lambda (a b)
     (string=
      (cdr a)
      (cdr b)))))


(provide "oldtype/page")
