;;;
;;; oldtype/rss - an ad-hoc RSS generation routine for WiLiKi
;;;
;;;  Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
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
;;; Modified by kiyoka to implement OldType rss generator.
;;; I renamed namespace of wiliki- 'oldtype-' to avoid collision of
;;; installation.

;; In future, this might be rewritten to use proper XML framework.
;; for now, I use an ad-hoc approach.

(define-module oldtype.rss
  (use util.list)
  (use text.html-lite)
  (extend oldtype))
(select-module oldtype.rss)

;;
;; generate RSS content.
;;
;; header:
;;   (
;;     (url   . TITLE-OF-RSS)
;;     (title . TITLE-OF-RSS)
;;     (desc  . DESCRIPTION-OF-RSS)
;;   )
;; entries:
;;   (
;;     ;; entry No.1
;;     (
;;       (url    . FULL-URL)
;;       (utc    . GENERATE-DATE)
;;       (title  . TITLE)
;;     )
;;     ;; entry No.2
;;     .
;;   .
;;   .
;;  )
;;
(define (rss-format header entries)
  (let* (
         (title (assq-ref header 'title))
         (url   (assq-ref header 'url)))
    `("Content-type: text/xml\n\n"
      "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n"
      "<rdf:RDF
       xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
       xmlns=\"http://purl.org/rss/1.0/\"
       xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
      >\n"
      ,(rdf-channel
        url
        (rdf-title title)
        (rdf-link  url)
        (rdf-description desc)
        (rdf-items-seq
         (map (lambda (entry) (rdf-li (assq-ref entry 'title))
              entries))))
      ,(map (lambda (entry)
              (let1 url (assq-ref entry 'url)
                    (rdf-item url
                              (rdf-title (assq-ref entry 'url))
                              (rdf-link url)
                              (dc-date   (assq-ref entry 'utc)))))
            entries)
      "</rdf:RDF>\n")))

;; NB: these should be implemented within xml framework
(define (rdf-channel about . content)
  `("<channel rdf:about=\"" ,(html-escape-string about) "\">"
    ,@content
    "\n</channel>\n"))

(define (rdf-li resource)
  `("<rdf:li rdf:resource=\"" ,(html-escape-string resource) "\" />\n"))

(define (rdf-simple tag . content)
  `("<" ,tag ">" ,@content "</" ,tag ">\n"))
(define (rdf-item about . content)
  `("<item rdf:about=\"" ,(html-escape-string about) "\">"
    ,@content
    "</item>\n"))

(define (rdf-items-seq . items)
  `("<items><rdf:Seq>" ,@items "</rdf:Seq></items>\n"))

(define (rdf-simple-1 tag content)
  `("<" ,tag ">" ,(html-escape-string content) "</" ,tag ">\n"))

(define (rdf-title title) (rdf-simple-1 "title" title))
(define (rdf-link link) (rdf-simple-1 "link" link))
(define (rdf-description desc) (rdf-simple-1 "description" desc))

(define (dc-date secs)
  (rdf-simple-1 "dc:date"
                (sys-strftime "%Y-%m-%dT%H:%M:%S+00:00" (sys-gmtime secs))))

(provide "oldtype/rss")
