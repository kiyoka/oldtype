;;;
;;; oldtype/kahualib.scm - oldtype utility for kahua application
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

(define-module oldtype.kahualib
  (use srfi-1)
  (use file.util)
  (use oldtype.page)
  (export 
   oldtype:load-page
   ))
(select-module oldtype.kahualib)

;;=================================================
;; Utility for OldType Application on Kahua
;;
(define (oldtype:load-page _site-root wikiname)
  (define (gen-sexp-filename)
    (string-append _site-root "/tmp/oldtype/_out/" wikiname ".sexp"))
  (if (file-exists? (gen-sexp-filename))
      (with-input-from-file (gen-sexp-filename)
        (lambda ()
          (deserialize
           (make <oldtype-page>)
           (read (current-input-port)))))
      #f))


(provide "oldtype/kahualib")
