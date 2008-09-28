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
          oldtype:sxml->plain-text
          ))
(select-module oldtype.format)


;;=================================================
;; Formatting: Wiki -> SXML
;;

;; Utility to generate a (mostly) unique id for the headings.
;; Passes a list of heading string stack.
(define (oldtype:calculate-heading-id headings)
  (string-append "H-" (number->string (hash headings) 36)))


(define (oldtype:wikiname->plain wikiname)
  (let1 m (#/[|](.+)/ wikiname)
        (if m
            (rxmatch-substring m 1)
            wikiname)))

(define (oldtype:wiki-macro->plain expr)
  (let ((command (car expr))
        (arg  ;; symbol list to string list.
         (map
          (lambda (x)
            (if (number? x)
                (number->string x)
                (symbol->string x)))
          (cdr expr)))
        (len (length (cdr expr))))
    (case command
      ((img)        "[img] ")
      ((todo)       "[TODO] ")
      ((done)       "[DONE] ")
      ((youtube)    "[YouTube] ")
      ((thumb)
       (if (< 0 len)
           (let1 _url (car arg)
                 (string-append "[Thumb " _url "] "))
           "!!Error : No argument ##(thumb URL) command"))
      ((amazon)
       (if (< 0 len)
           (let* ((_asin (car arg))
                  (_asin (if (#/^[0-9]+$/ _asin)
                             (format "~10,,,'0@a" _asin)
                             _asin)))
             (if (#/^[0-9a-zA-Z]+$/ _asin)
                 (format "[Amazon ~a] " _asin)
                 (format "!!Error : ASIN code format error for ##(amazon asin) command \"~a\"!!" _asin)))
           "!!Error : No argument ##(amazon URL) command"))
      ;; timestamp
      ((timestamp)   "[timestamp] ")
      ;; since
      ((since)       "[since] ")
      ;; download link oldtype-mode.el source code
      ((download-el) "[Download oldtype-mode.el now]")
      ;; comment-data
      ((comment-data)
       (if (< 1 len)
           (string-append (format "----< ~a >----" (uri-decode-string (car arg)))
                          "\n"
                          (uri-decode-string (cadr arg)))
           (format "!!Error : comment-data format error for ##(comment-data user string) command")))
      ;; else
      (else
       (format "[~a]" command)))))

;;
;; SXML to wiki-command list in the page.
;;   TODO: unit-test
;;
(define (oldtype:sxml->command-list sxmls)
  (let* (
         (commands '())
         (sxmls sxmls))
    (match sxmls
           (()  '())
           (((and (name . _) sxml) . rest) ;; generic node
            (let1 arg (cdr sxml)
                  (cons
                   (case name
                     ((div)
                      (let* ((param (car arg)) ;; param is assoc-list
                             (lineno (assq-ref param 'lineno)))
                        (rec (cdr arg))))
                     ((a)
                      (let1 param (car arg) ;; param is assoc-list
                            (rec (cdr arg))))
                     ((p-normal pre-quote pre-verb pre-ul1 pre-ul2 pre-ul3 pre-ol1 pre-ol2 pre-ol3 h1 h2 h3 h4 h5 h6)
                      (rec arg))
                     ((hr @ @@)
                      '())
                     ((wiki-macro)
                      (push! commands (car arg)))
                     ((wiki-name)
                      (push! commands (car arg)))
                     (else
                      (format "!!Error : no such tag \"~a\"!!" name)))
                   (rec rest))))
           ((other . rest)
            (cons other (rec rest))))
    commands))


;;
;; return:
;;   ;;   plain-text
;;   ( "string of plain text of sxmls"
;;     ;; list of local-link(wikiname) in this page.
;;     ( "wikiname1" "wikiname2")
;;   )
;;
(define (oldtype:sxml->plain-text sxmls)
  (tree->string
   (let rec
       ((sxmls sxmls))
     (match sxmls
            (()  '())
            (((and (name . _) sxml) . rest) ;; generic node
             (let1 arg (cdr sxml)
                   (cons
                    (case name
                      ((div)
                       (let* ((param (car arg)) ;; param is assoc-list
                              (lineno (assq-ref param 'lineno)))
                         (rec (cdr arg))))
                      ((a)
                       (let1 param (car arg) ;; param is assoc-list
                             (rec (cdr arg))))
                      ((p-normal)    (rec arg))
                      ((pre-quote)   (rec arg))
                      ((pre-verb)    (rec arg))
                      ((pre-ul1)     (cons "- "     (rec arg)))
                      ((pre-ul2)     (cons "-- "    (rec arg)))
                      ((pre-ul3)     (cons "--- "   (rec arg)))
                      ((pre-ol1)     (cons "# "     (rec arg)))
                      ((pre-ol2)     (cons "## "    (rec arg)))
                      ((pre-ol3)     (cons "### "   (rec arg)))
                      ((h1)          (cons "[] "    (rec arg)))
                      ((h2)          (cons "* "     (rec arg)))
                      ((h3)          (cons "** "    (rec arg)))
                      ((h4)          (cons "*** "   (rec arg)))
                      ((h5)          (cons "**** "  (rec arg)))
                      ((h6)          (cons "***** " (rec arg)))
                      ((wiki-macro)  (oldtype:wiki-macro->plain arg))
                      ((wiki-name)   (oldtype:wikiname->plain (car arg)))
                      ((hr)          (list "----\n"))
                      ((@)           '())
                      ((@@)          '())
                      (else
                       (format "!!Error : no such tag \"~a\"!!" name)))
                    (rec rest))))
            ((other . rest)
             (cons other (rec rest)))))))



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
