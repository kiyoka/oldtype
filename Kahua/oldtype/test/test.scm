;; -*- coding: utf-8; mode: scheme -*-
;; test oldtype script.
;; $Id:$

(use gauche.test)
(use file.util)
(use text.tree)
(use sxml.ssax)
(use sxml.sxpath)
(use kahua)
(use kahua.test.xml)
(use kahua.test.worker)

(test-start "oldtype")

(define GOSH "/usr/local/bin/gosh")
(define *PLUGINS* '(oldtype.scm))
(define *config* "test.conf")

(sys-system "rm -rf _tmp _work oldtype")
(sys-mkdir "oldtype" #o755)
(sys-mkdir "_tmp" #o755)
(sys-mkdir "_work" #o755)
(sys-mkdir "_work/plugins" #o755)
(sys-mkdir "_work/templates" #o755)
(sys-mkdir "_work/templates/oldtype" #o755)

(copy-file "../plugins/oldtype.scm"  "_work/plugins/oldtype.scm")
(copy-file "../templates/page.xml"  "_work/templates/oldtype/page.xml")

(kahua-init *config*)

;;------------------------------------------------------------
;; Run oldtype
(test-section "kahua-server oldtype.kahua")

(with-worker
 (w `(,GOSH "-I.." "-I/usr/local/kahua/lib/kahua" "kahua-server.scm" "-c" ,*config*
            "../oldtype/oldtype.kahua"))

 (test* "run oldtype.kahua" #t (worker-running? w))


;  (test* "oldtype"
;         '(html (head (title "Hello, world!"))
;                (body (h1 "Hello, world!")
;                      (a (@ (href ?&)) "version")))
;         (call-worker/gsid w '() '() (lambda (h b) (tree->string b)))
;         (make-match&pick w))

;  (test* "version"
;        '(html (head (title "oldtype: version 0.0.0"))
;               (body (h1 "oldtype: version 0.0.0")
;                     (a ?@ ?*)))
;        (call-worker/gsid w '() '() (lambda (h b) (tree->string b)))
;        (make-match&pick w))

 )

(test-end)

