;; -*- coding: utf-8 -*-
(use srfi-1)
(use sxml.tools)
(use sxml.serializer)
(use text.html-lite)
(use text.tree)
(use gauche.charconv)
(use oldtype.parse)
(use oldtype.format)
(use oldtype.util)
(use oldtype.log)
(use oldtype.timeline)
(use oldtype.page)
(use oldtype.core)
(use oldtype.svn)
(use gauche.test)                                                     
(use util.list)


;; Main -------------------------------------------------------
(define (main args)
  (let* (
	 (_ (cdr args))
	 (input-file (first _))
         (log-file   (second _))
         (ann-file   (third _))
         (converted-str
	  (port->string
	   (open-input-conversion-port
            (open-input-file input-file)
	    "*JP")))

	 ;; Making string port from stdin/file
	 ;;
	 (input-port
	  (open-input-string converted-str)))
    (let ((oldtype-page #f)
          (oldtype-page-no-timeline #f)
          (oldtype-timeline #f)
          (loaded
           (with-input-from-file "Test.sexp.master"
             (lambda ()
               (read))))
          (loaded-no-timeline
           (with-input-from-file "Test.no-timeline.sexp.master"
             (lambda ()
               (read)))))
      (test-start "serialize,deserialize")

      (test-section "oldtype-timeline")
      (let1 oldtype-timeline 
            (parse (make <oldtype-timeline> :name "Test") log-file ann-file)
            (let* ((serialized     (serialize oldtype-timeline))
                   (deserialized   (deserialize (make <oldtype-timeline>) serialized)))
              (test "serialized == DATA        "
                    (assq-ref loaded 'timeline)
                    (lambda () (serialize deserialized)))
              (test "serialized == deserialized" serialized (lambda () (serialize deserialized)))))

      (test-section "oldtype-page")
      (set! oldtype-page
            (parse (make <oldtype-page> :name "Test") input-port log-file ann-file))
      (port-seek input-port 0)
      (set! oldtype-page-no-timeline
            (parse (make <oldtype-page> :name "Test") input-port #f #f))

      (let1 serialized     (serialize oldtype-page)
            (test "serialized == DATA (1)  "
                  loaded
                  (lambda () (serialize oldtype-page)))
            
            (test "serialized == deserialized" serialized (lambda () 
                                                            (serialize
                                                             (deserialize
                                                              (make <oldtype-page>)
                                                              serialized))))
            (test "serialized == DATA (2)  "
                  loaded-no-timeline
                  (lambda () (serialize oldtype-page-no-timeline))))
      (test-end)


      (test-start "oldtype-page util method")
      
      (test-section "oldtype-timeline")
      (set! oldtype-timeline (timeline-of oldtype-page))
      (test "log of lineno 1"
            '((revision . 8208) (committer . kiyoka) (utc . 1206016615) (rank . 5))
            (lambda ()
              (serialize (log-by-lineno oldtype-timeline 1))))

      (test "ago string of lineno 1 <oldtype-timeline>"
            "  (3 months ago)" 
            (lambda ()
              (get-ago (log-by-lineno oldtype-timeline 1))))
      (test "ago string of lineno 1 <oldtype-page>"
            "  (3 months ago)"
            (lambda ()
              (get-ago oldtype-page 1)))
      
      (test "date string of lineno 1 <oldtype-timeline>"
            "2008-03-20 21:36 (+0900)"
            (lambda ()
              (get-date (log-by-lineno oldtype-timeline 1))))
      (test "date string of lineno 1 <oldtype-page>"
            "2008-03-20 21:36 (+0900)"
            (lambda ()
              (get-date oldtype-page 1)))

      (test "rank value of lineno 1 <oldtype-timeline>"
            5
            (lambda ()
              (rank-by-lineno oldtype-timeline 1)))
      (test "rank value of lineno 2 <oldtype-timeline>"
            4
            (lambda ()
              (rank-by-lineno oldtype-timeline 2)))

      (test "date,ago,rank,committer of lineno 1 <oldtype-page>"
            '((date  . "2008-03-20 21:36 (+0900)")
              (ago   . "  (3 months ago)")
              (rank  . 5)
              (committer . "kiyoka"))
            (lambda ()
              `(
                (date      . ,(get-date      oldtype-page 1))
                (ago       . ,(get-ago       oldtype-page 1))
                (rank      . ,(get-rank      oldtype-page 1))
                (committer . ,(get-committer oldtype-page 1)))))

      (test "text of lineno 1 <oldtype-page>"
            "* UnitTest用のサンプルファイル"
            (lambda ()
              (get-text oldtype-page 1)))
      
      (test "text of lineno 2 <oldtype-page>"
            "----"
            (lambda ()
              (get-text oldtype-page 2)))

      (test "rank-list of <oldtype-page>"
            '(5 4 3 3 3 3)
            (lambda ()
              (get-rank-list oldtype-page)))

      
      (test-end)

      (test-start "generating RSS")
      
      (test "wikiname list for RSS"
            '((4 . "Entry1") (5 . "Entry2"))
            (lambda ()
              (get-rss-entry-pages oldtype-page)))

      (test "oldtype-page list for RSS"
            '(("Entry1" "Entry1の1行目" ((revision . 13304) (committer . kiyoka) (utc . 1212675369) (rank . 5)))
              ("Entry2" "Entry2の1行目" ((revision . 13334) (committer . kiyoka) (utc . 1212756234) (rank . 4))))
            (lambda ()
              (map
               (lambda (x)
                 (let* ((page (oldtype:load-page "" (cdr x)))
                        (timeline (timeline-of page)))
                   `(
                     ,(name-of page)
                     ,(car (get-text-list page))
                     ,(serialize 
                       (get-latest-log timeline)))))
               (get-rss-entry-pages oldtype-page))))

      (test "lines of latest revision in Test.ot"
            '(6 5 4 3)
            (lambda ()
              (get-latest-lines oldtype-timeline)))

      (test "The page that doesn't have RSS data."
            '()
            (lambda ()
              (get-rss-entry-pages 
               (oldtype:load-page "" "Entry1"))))

      (test-end)

      (test-start "svn commit")

      (let1 work
            (make <svn-work> :url "http://genkan.sumibi.org/svn/newtype" :user "anonymous" :pass "anonymous" :basepath "/Users/kiyoka/work/tmp")
            
            (test "Initialize svn work directory"
                  #t
                  (lambda ()
                    (string? (init work "123"))))

            (test "status of wikiname (no changes)"
                  #f
                  (lambda ()
                    (status work "_kiyoka")))

            (test "status of wikiname (some changes)"
                  "M"
                  (lambda ()
                    (sys-system (format "echo 'a' >> ~a/~a/~a" (get-fullpath work) "edit" "test.ot"))
                    (car (status work "test"))))
            
            (when
                #f
              (test "commit from work"
                    #t
                    (lambda ()
                      (commit work)))))
      
      (test-end)
      
      )))
