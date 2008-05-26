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
(use slib)
(require 'pretty-print)
(use gauche.test)                                                     


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
          (oldtype-timeline #f))

      (test-start "serialize,deserialize")

      (test-section "oldtype-timeline")
      (let1 oldtype-timeline 
            (parse (make <oldtype-timeline>) log-file ann-file)
            (let* ((serialized     (serialize oldtype-timeline))
                   (deserialized   (deserialize (make <oldtype-timeline>) serialized)))
              (test "serialized == DATA        "
                    '((name . "none") (revision . 8208)
                      (log
                       (8208 (revision . 8208) (committer . kiyoka) (utc . 1206016615) (rank . 0))
                       (8205 (revision . 8205) (committer . kiyoka) (utc . 1206012635) (rank . 1))
                       (8091 (revision . 8091) (committer . kiyoka) (utc . 1205842976) (rank . 2))
                       (7924 (revision . 7924) (committer . kiyoka) (utc . 1205421544) (rank . 3))
                       (7920 (revision . 7920) (committer . kiyoka) (utc . 1205419584) (rank . 4))
                       (7919 (revision . 7919) (committer . kiyoka) (utc . 1205418381) (rank . 5))
                       (7870 (revision . 7870) (committer . kiyoka) (utc . 1205336331) (rank . 6))
                       (7811 (revision . 7811) (committer . kiyoka) (utc . 1205239814) (rank . 7)))
                      (annotation
                       ((revision . 8208) (committer . kiyoka) (utc . 1206016615) (rank . 0))
                       ((revision . 7811) (committer . kiyoka) (utc . 1205239814) (rank . 7)))
                      (text "* UnitTest用のサンプルファイル" ""))
                    (lambda () (serialize deserialized)))
              (test "serialized == deserialized" serialized (lambda () (serialize deserialized)))))

      (test-section "oldtype-page")
      (set! oldtype-page (parse (make <oldtype-page>) input-port log-file ann-file))
      (let1 serialized     (serialize oldtype-page)
            (test "serialized == DATA        "
                  '((name . "none")
                    (sxml
                     (div
                      ((lineno . 1))
                      (h2 "UnitTest用のサンプルファイル" "\n")))
                    (timeline
                     (name . "none") (revision . 8208)
                     (log
                      (8208 (revision . 8208) (committer . kiyoka) (utc . 1206016615) (rank . 0))
                      (8205 (revision . 8205) (committer . kiyoka) (utc . 1206012635) (rank . 1))
                      (8091 (revision . 8091) (committer . kiyoka) (utc . 1205842976) (rank . 2))
                      (7924 (revision . 7924) (committer . kiyoka) (utc . 1205421544) (rank . 3))
                      (7920 (revision . 7920) (committer . kiyoka) (utc . 1205419584) (rank . 4))
                      (7919 (revision . 7919) (committer . kiyoka) (utc . 1205418381) (rank . 5))
                      (7870 (revision . 7870) (committer . kiyoka) (utc . 1205336331) (rank . 6))
                      (7811 (revision . 7811) (committer . kiyoka) (utc . 1205239814) (rank . 7)))
                     (annotation
                      ((revision . 8208) (committer . kiyoka) (utc . 1206016615) (rank . 0))
                      ((revision . 7811) (committer . kiyoka) (utc . 1205239814) (rank . 7)))
                     (text "* UnitTest用のサンプルファイル" "")))
                  (lambda () (serialize oldtype-page)))

            (test "serialized == deserialized" serialized (lambda () 
                                                            (serialize
                                                             (deserialize
                                                              (make <oldtype-page>)
                                                              serialized)))))
      (test-end)


      (test-start "oldtype-page util method")
      
      (test-section "oldtype-timeline")
      (set! oldtype-timeline (timeline-of oldtype-page))
      (test "log of lineno 1"
            '((revision . 8208) (committer . kiyoka) (utc . 1206016615) (rank . 0))            
            (lambda ()
              (serialize (log-by-lineno oldtype-timeline 1))))

      (test "ago string of lineno 1 <oldtype-timeline>"
            "  (2 months ago)" 
            (lambda ()
              (ago (log-by-lineno oldtype-timeline 1))))
      (test "ago string of lineno 1 <oldtype-page>"
            "  (2 months ago)"
            (lambda ()
              (ago oldtype-page 1)))
      
      (test "date string of lineno 1 <oldtype-timeline>"
            "2008-03-20 21:36 (+0900)"
            (lambda ()
              (date (log-by-lineno oldtype-timeline 1))))
      (test "date string of lineno 1 <oldtype-page>"
            "2008-03-20 21:36 (+0900)"
            (lambda ()
              (date oldtype-page 1)))

      (test "rank value of lineno 1 <oldtype-timeline>"
            0
            (lambda ()
              (rank-by-lineno oldtype-timeline 1)))
      (test "rank value of lineno 2 <oldtype-timeline>"
            7
            (lambda ()
              (rank-by-lineno oldtype-timeline 2)))

      (test-end)
      )))
