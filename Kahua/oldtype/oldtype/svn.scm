;;;
;;; oldtype/svn.scm - svn access for OldType
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

(define-module oldtype.svn
  (use srfi-1)
  (use srfi-13)
  (use srfi-19)
  (use sxml.ssax)
  (use sxml.sxpath)
  (use text.parse)
  (use file.util)
  (use util.list)
  (use gauche.process)
  (export init
          commit
          status
          get-fullpath
          save-text-list
          <svn-work>
          ))
(select-module oldtype.svn)


(define-class <svn-work> ()
  (;; Customization parameters -----------------------
   
   ;; url of subversion ( like http://host/a/b/c/ )
   (url         :accessor url-of         :init-keyword :url
                :init-value "")
   ;; full-path of svn-work
   (basepath    :accessor basepath-of    :init-keyword :basepath
                :init-value "")
   ;; relative-path of svn-work
   (path        :accessor path-of        :init-keyword :path
                :init-value "")
   ;; user of svn account
   (user        :accessor user-of        :init-keyword :user
                :init-value '())
   ;; pass of svn account
   (pass        :accessor pass-of        :init-keyword :pass
                :init-value '())
   ))


(define-method init ((self <svn-work>) session-id)
  (set! (path-of self) session-id)
  ;; /bin/rm if already exist
  (call-with-input-process
   (format "/bin/rm -rf ~a" (get-fullpath self))
   (lambda (p)
     (display (port->string p))))
  ;; mkdir 
  (make-directory* (get-fullpath self))
  ;; checkout
  (_co self))



(define-method _co ((self <svn-work>))
  (call-with-input-process
   (format "cd ~a ; svn co --no-auth-cache --username ~a --password ~a ~a/edit"
           (get-fullpath self)
           (user-of self) (pass-of self)
           (url-of self))
   (lambda (p)
     (let1 str (port->string-list p)
           (display (port->string p))
           (last str)))))


(define-method get-fullpath ((self <svn-work>))
  (string-append (basepath-of self) "/" (path-of self)))


(define-method status ((self <svn-work>) wikiname)
  (call-with-input-process
   (format "cd ~a ; svn status --non-interactive --no-auth-cache --username ~a --password ~a edit/~a"
           (get-fullpath self)
           (user-of self) (pass-of self)
           (string-append wikiname ".ot"))
   (lambda (p)
     (let1 str (port->string p)
           (display str)
           (if (string= "" str)
               '("" "")
               (string-split str #/[ ]+/))))))


(define-method commit ((self <svn-work>))
  (call-with-input-process
   (format "cd ~a ; svn commit --non-interactive --no-auth-cache -m 'commited with svn.scm' --username ~a --password ~a edit/"
           (get-fullpath self)
           (user-of self) (pass-of self))
   (lambda (p)
     (display (port->string p))))
  #t)


(define-method save-text-list ((self <svn-work>) wikiname text-list)
  (with-output-to-file (format "~a/edit/~a.ot" (get-fullpath self) wikiname)
    (lambda ()
      (for-each
       (lambda (str)
         (display str)
         (newline))
       text-list))))


(provide "oldtype/svn")
