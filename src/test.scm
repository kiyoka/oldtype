;; -*- coding: utf-8 -*-
(use oldtype.parse)
(use oldtype.format)
(use srfi-1)
(use slib)
(use sxml.tools)
(use sxml.serializer)
(use text.html-lite)
(use text.tree)
(require 'pretty-print)

(define (test:sxml->html sxml)
  (let1 html-body (srl:sxml->html sxml)
        (tree->string
         `(
           ,(html:html                                                      
             (html:head
              (html:meta :http-equiv "Content-Type"        :content "text/html; charset=utf-8")
              (html:meta :http-equiv "Content-Style-Type"  :content "text/css")
              (html:link :rel "stylesheet" :href "oldtype.css" :type "text/css")
              (html:title "テストページ")
              (html:body
               html-body
               )))))))

(let* (
       (sxml           (oldtype-parse (current-input-port)))
       ;; (expanded-sxml  (oldtype:expand-page sxml))
       ;; (html           (test:sxml->html expanded-sxml))
       )
  (pretty-print sxml))





      
  



