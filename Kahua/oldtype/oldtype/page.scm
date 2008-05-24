;;;
;;; oldtype/page - wiki page infomation
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
;; it contains multi <oldtype-line> infomation
;;

(define-module oldtype.page
  (use util.list)
  (use oldtype.timeline)
  (use oldtype.parse)
  (use oldtype.format)
  (use oldtype.util)
  (export <oldtype-page>
          serialize
          deserialize
          parse
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
   ))


(define-method parse ((self <oldtype-page>) wiki-port log-file ann-file)
  (let1 timeline (make <oldtype-timeline> :name (name-of self))
        (set! (timeline-of self)
              (parse timeline log-file ann-file)))
  (set! (sxml-of self)
        (oldtype:sxml->internal
         (oldtype-parse wiki-port)))
  self)


(define-method serialize ((self <oldtype-page>))
  `(
    (name     . ,(name-of     self))
    (sxml     . ,(sxml-of     self))
    (timeline . ,(serialize (timeline-of self)))))


(define-method deserialize ((dummy <oldtype-page>) internal-data)
  (make <oldtype-page>
    :name      (assq-ref internal-data 'name)
    :sxml      (assq-ref internal-data 'sxml)
    :timeline  (deserialize (make <oldtype-timeline>) (assq-ref internal-data 'timeline))))


(provide "oldtype/page")
