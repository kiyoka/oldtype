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
;;; $Id: format.scm 289 2008-03-01 01:50:33Z kiyoka $
;;;
;;; Modified by kiyoka to implement OldType wiki formatter.
;;; I renamed namespace of wiliki- 'oldtype-' to avoid collision of
;;; installation.
;;;

(define-module oldtype.format
  (use srfi-1)
  (use srfi-2)
  (use srfi-11)
  (use srfi-13)
  (use text.html-lite)
  (use text.tree)
  (use text.tr)
  (use rfc.uri)
  (use util.list)
  (use util.queue)
  (use util.match)
  (use gauche.parameter)
  (use gauche.charconv)
  (use gauche.sequence)
  (use oldtype.parse)
  (use oldtype.util)
  (use sxml.tools)
  (export oldtype:sxml->internal
          oldtype:expand-page
          oldtype:format-line-plainly
          )
  )
(select-module oldtype.format)


;;=================================================
;; Formatting: Wiki -> SXML
;;

;; Utility to generate a (mostly) unique id for the headings.
;; Passes a list of heading string stack.
(define (oldtype:calculate-heading-id headings)
  (string-append "H-" (number->string (hash headings) 36)))

;; utility : strips wiki markup and returns a plaintext line.
(define (oldtype:format-line-plainly line)
  (reverse! ((rec (tree-fold tree seed)
               (match tree
                 ("\n" seed)  ;; skip newline
                 ((? string?) (cons tree seed))
                 (('@ . _)  seed)  ;; skip attr node
                 (('@@ . _) seed)  ;; skip aux node
                 (('wiki-name name) (cons name seed))
                 (('wiki-macro . _) seed)
                 ((name . nodes) 
                  (fold tree-fold seed nodes))
                 (else seed)))
             `(x ,@(oldtype-parse-string line))
             '())))


;;
;; (REV-NO
;;   ((user . oldtype)
;;    (str  . "line of .ot file")
;;   )
;; )
;;
(define (get-ann-by-lineno ann lineno)
  (let1 by-lineno
        (map-with-index
         (lambda (index body)
           (cons (+ index 1)
                 body))
         ann)
        (assq-ref by-lineno lineno)))

;; 
;; [SXML] -> [SEXP for Kahua], parsed sxml to internal format.
;; 
;; [intenal format is SXML like]
;;  (
;;    (div
;;      ((lineno      . LINENO))
;;      (TAG
;;         "string" "string" "string"
;;         (wiki-macro MACRONAME args)
;;         (wiki-name  WIKINAME)
;;         (TAG ..... )
;;      )
;;    )
;;    (div
;;      .
;;      .
;;    )
;;  )
;;
;;  TAG is h1, h2, h3, h4, p, pre ...
;;
;; 
(define (oldtype:sxml->internal sxmls)
  (let rec
      ((sxmls sxmls)
       (hctx '())) ;;headings context
    (match sxmls
           (()  '())
           ((('wiki-macro . expr) . rest)
            (cons `(wiki-macro ,@expr)
                  (rec rest hctx)))
           (((and (name . _) sxml) . rest) ;; generic node
            (let* ((lineno      (assq 'lineno (sxml:aux-list-u sxml))))
              (let1 _
                    `(,name ,@(cond ((sxml:attr-list-node sxml) => list)
                                    (else '()))
                            ,@(rec (sxml:content sxml) hctx))
                    (cons
                     (case name
                       ((div)
                        `(div
                          (,(if lineno
                                `(lineno  . ,(second lineno))
                                `()))
                          ,@(rec (sxml:content sxml) hctx)))
                       ((a)
                        (let ((param (cadr sxml))
                              (_rest (cddr sxml)))
                          `(a
                            ,param
                            ,@(rec _rest hctx))))
                       (else
                        _))
                     (rec rest hctx)))))
           ((other . rest)
            (cons other (rec rest hctx))))))


;; 
;; [SXML] -> [SXML], expanding wiki-name and wiki-macro nodes.
;; 
(define (oldtype:expand-page sxmls log ann)
  (let1 top (if ann
                (top-revisions log ann)
                '())
        (let rec
            ((sxmls sxmls)
             (hctx '())) ;;headings context
          (match sxmls
                 (()  '())
                 ((('wiki-name name) . rest)
                  (append (oldtype:expand-wiki-name name)
                          (rec rest hctx)))
                 ((('wiki-macro . expr) . rest)
                  (append (oldtype:format-macro expr)
                          (rec rest hctx)))
                 (((and ((or 'h2 'h3 'h4 'h5 'h6) . _) sxml) . rest)
                  ;; extract heading hierarchy to calculate heading id
                  (let* ((hn   (sxml:name sxml))
                         (lineno (assq 'lineno (sxml:aux-list-u sxml)))
                         (hkey (assq 'hkey (sxml:aux-list-u sxml)))
                         (hctx2 (extend-headings-context hctx hn hkey)))
                    (cons `(,hn
                            ;; disable hkey (WiLiKi's feature)
                            ;;  ,@(if hkey
                            ;;     `((@ (id ,(heading-id hctx2))))
                            ;;     '())
                            ,@(rec (sxml:content sxml) hctx)
                            ,@(if lineno
                                  `((@ (id ,(second lineno))))
                                  '()))
                          (rec rest hctx2))))
                 (((and (name . _) sxml) . rest) ;; generic node
                  (let* ((lineno     (assq 'lineno (sxml:aux-list-u sxml)))
                         (line-alist (if (and lineno ann (get-ann-by-lineno ann (second lineno)))
                                         (car (assq-ref
                                               top
                                               (car (get-ann-by-lineno ann (second lineno)))))
                                         '((index . 5))))
                         (new-level (assq-ref line-alist 'index)))
                    (cons `(,name ,@(cond ((sxml:attr-list-node sxml) => list)
                                          (else '()))
                                  ,@(rec (sxml:content sxml) hctx)
                                  ,@(if lineno
                                        `((@ (id ,(second lineno))
                                             (class ,(format "new~d" new-level))))
                                        '()))
                          (rec rest hctx))))
                 ((other . rest)
                  (cons other (rec rest hctx)))))))


(define (hn->level hn)
  (find-index (cut eq? hn <>) '(h2 h3 h4 h5 h6)))

(define (extend-headings-context hctx hn hkey)
  (if (not hkey)
    hctx
    (let* ((level (hn->level hn))
           (up (drop-while (lambda (x) (>= (hn->level (car x)) level)) hctx)))
      (acons hn (cadr hkey) up))))

(define (heading-id hctx)
  (oldtype:calculate-heading-id (map cdr hctx)))



(provide "oldtype/format")
