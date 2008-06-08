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
            (parse (make <oldtype-timeline> :name "Test") log-file ann-file)
            (let* ((serialized     (serialize oldtype-timeline))
                   (deserialized   (deserialize (make <oldtype-timeline>) serialized)))
              (test "serialized == DATA        "
                    '((name . "Test") (revision . 13317)
                      (log
                       (13334 (revision . 13334) (committer . kiyoka) (utc . 1212756234) (rank . 5))
                       (13317 (revision . 13317) (committer . kiyoka) (utc . 1212711085) (rank . 3))
                       (13306 (revision . 13306) (committer . kiyoka) (utc . 1212676363) (rank . 5))
                       (13304 (revision . 13304) (committer . kiyoka) (utc . 1212675369) (rank . 4))
                       (13242 (revision . 13242) (committer . kiyoka) (utc . 1212594143) (rank . 5))
                       (13237 (revision . 13237) (committer . kiyoka) (utc . 1212592559) (rank . 5))
                       (13233 (revision . 13233) (committer . kiyoka) (utc . 1212591037) (rank . 5))
                       (13224 (revision . 13224) (committer . kiyoka) (utc . 1212584569) (rank . 5))
                       (13223 (revision . 13223) (committer . kiyoka) (utc . 1212584402) (rank . 5))
                       (13080 (revision . 13080) (committer . kiyoka) (utc . 1212407693) (rank . 5))
                       (13032 (revision . 13032) (committer . kiyoka) (utc . 1212334026) (rank . 5))
                       (13023 (revision . 13023) (committer . kiyoka) (utc . 1212331805) (rank . 5))
                       (12471 (revision . 12471) (committer . kiyoka) (utc . 1211813046) (rank . 5))
                       (12460 (revision . 12460) (committer . kiyoka) (utc . 1211806608) (rank . 5))
                       (12356 (revision . 12356) (committer . kiyoka) (utc . 1211721110) (rank . 5))
                       (12293 (revision . 12293) (committer . kiyoka) (utc . 1211642182) (rank . 5))
                       (12266 (revision . 12266) (committer . kiyoka) (utc . 1211601688) (rank . 5))
                       (11607 (revision . 11607) (committer . kiyoka) (utc . 1210777124) (rank . 5))
                       (11606 (revision . 11606) (committer . kiyoka) (utc . 1210777098) (rank . 5))
                       (11603 (revision . 11603) (committer . kiyoka) (utc . 1210774733) (rank . 5))
                       (11245 (revision . 11245) (committer . kiyoka) (utc . 1210161037) (rank . 5))
                       (10961 (revision . 10961) (committer . kiyoka) (utc . 1209740697) (rank . 5))
                       (10957 (revision . 10957) (committer . kiyoka) (utc . 1209737179) (rank . 5))
                       (10149 (revision . 10149) (committer . kiyoka) (utc . 1208864713) (rank . 5))
                       (10146 (revision . 10146) (committer . kiyoka) (utc . 1208862687) (rank . 5))
                       (10084 (revision . 10084) (committer . kiyoka) (utc . 1208784928) (rank . 5))
                       (10014 (revision . 10014) (committer . kiyoka) (utc . 1208695655) (rank . 5))
                       (9977 (revision . 9977) (committer . kiyoka) (utc . 1208667401) (rank . 5))
                       (9976 (revision . 9976) (committer . kiyoka) (utc . 1208667383) (rank . 5))
                       (9952 (revision . 9952) (committer . kiyoka) (utc . 1208652380) (rank . 5))
                       (9948 (revision . 9948) (committer . kiyoka) (utc . 1208650871) (rank . 5))
                       (9924 (revision . 9924) (committer . kiyoka) (utc . 1208619757) (rank . 5))
                       (9903 (revision . 9903) (committer . kiyoka) (utc . 1208606479) (rank . 5))
                       (9585 (revision . 9585) (committer . kiyoka) (utc . 1208348091) (rank . 5))
                       (9583 (revision . 9583) (committer . kiyoka) (utc . 1208346288) (rank . 5))
                       (8962 (revision . 8962) (committer . kiyoka) (utc . 1207444336) (rank . 5))
                       (8676 (revision . 8676) (committer . kiyoka) (utc . 1207141582) (rank . 5))
                       (8607 (revision . 8607) (committer . kiyoka) (utc . 1207051192) (rank . 5))
                       (8569 (revision . 8569) (committer . kiyoka) (utc . 1206968180) (rank . 5))
                       (8342 (revision . 8342) (committer . kiyoka) (utc . 1206360044) (rank . 5))
                       (8263 (revision . 8263) (committer . kiyoka) (utc . 1206115042) (rank . 5))
                       (8254 (revision . 8254) (committer . kiyoka) (utc . 1206097354) (rank . 5))
                       (8212 (revision . 8212) (committer . kiyoka) (utc . 1206027766) (rank . 5))
                       (8211 (revision . 8211) (committer . kiyoka) (utc . 1206027177) (rank . 5))
                       (8208 (revision . 8208) (committer . kiyoka) (utc . 1206016615) (rank . 5))
                       (8205 (revision . 8205) (committer . kiyoka) (utc . 1206012635) (rank . 5))
                       (8091 (revision . 8091) (committer . kiyoka) (utc . 1205842976) (rank . 5))
                       (7924 (revision . 7924) (committer . kiyoka) (utc . 1205421544) (rank . 5))
                       (7920 (revision . 7920) (committer . kiyoka) (utc . 1205419584) (rank . 5))
                       (7919 (revision . 7919) (committer . kiyoka) (utc . 1205418381) (rank . 5))
                       (7870 (revision . 7870) (committer . kiyoka) (utc . 1205336331) (rank . 5))
                       (7811 (revision . 7811) (committer . kiyoka) (utc . 1205239814) (rank . 5)))
                      (annotation
                       ((revision . 8208) (committer . kiyoka) (utc . 1206016615) (rank . 5))
                       ((revision . 13304) (committer . kiyoka) (utc . 1212675369) (rank . 4))
                       ((revision . 13317) (committer . kiyoka) (utc . 1212711085) (rank . 3))
                       ((revision . 13317) (committer . kiyoka) (utc . 1212711085) (rank . 3))
                       ((revision . 13317) (committer . kiyoka) (utc . 1212711085) (rank . 3))
                       ((revision . 13317) (committer . kiyoka) (utc . 1212711085) (rank . 3)))
                      (text "* UnitTest用のサンプルファイル" "----" "*** start" "*** [[Entry1]]" "*** [[Entry2]]" "*** end"))
                    (lambda () (serialize deserialized)))
              (test "serialized == deserialized" serialized (lambda () (serialize deserialized)))))

      (test-section "oldtype-page")
      (set! oldtype-page (parse (make <oldtype-page> :name "Test") input-port log-file ann-file))
      (let1 serialized     (serialize oldtype-page)
            (test "serialized == DATA        "
                  '((name . "Test")
                    (sxml
                     (div
                      ((lineno . 1))
                      (h2 "UnitTest用のサンプルファイル" "\n"))
                     (div
                      ((lineno . 2))
                      (hr))
                     (div
                      ((lineno . 3))
                      (h4 "start" "\n"))
                     (div
                      ((lineno . 4))
                      (h4
                       (wiki-name "Entry1") "\n"))
                     (div
                      ((lineno . 5))
                      (h4
                       (wiki-name "Entry2") "\n"))
                     (div
                      ((lineno . 6))
                      (h4 "end" "\n")))
                    (timeline (name . "Test") (revision . 13317)
                              (log
                               (13334 (revision . 13334) (committer . kiyoka) (utc . 1212756234) (rank . 5))
                               (13317 (revision . 13317) (committer . kiyoka) (utc . 1212711085) (rank . 3))
                               (13306 (revision . 13306) (committer . kiyoka) (utc . 1212676363) (rank . 5))
                               (13304 (revision . 13304) (committer . kiyoka) (utc . 1212675369) (rank . 4))
                               (13242 (revision . 13242) (committer . kiyoka) (utc . 1212594143) (rank . 5))
                               (13237 (revision . 13237) (committer . kiyoka) (utc . 1212592559) (rank . 5))
                               (13233 (revision . 13233) (committer . kiyoka) (utc . 1212591037) (rank . 5))
                               (13224 (revision . 13224) (committer . kiyoka) (utc . 1212584569) (rank . 5))
                               (13223 (revision . 13223) (committer . kiyoka) (utc . 1212584402) (rank . 5))
                               (13080 (revision . 13080) (committer . kiyoka) (utc . 1212407693) (rank . 5))
                               (13032 (revision . 13032) (committer . kiyoka) (utc . 1212334026) (rank . 5))
                               (13023 (revision . 13023) (committer . kiyoka) (utc . 1212331805) (rank . 5))
                               (12471 (revision . 12471) (committer . kiyoka) (utc . 1211813046) (rank . 5))
                               (12460 (revision . 12460) (committer . kiyoka) (utc . 1211806608) (rank . 5))
                               (12356 (revision . 12356) (committer . kiyoka) (utc . 1211721110) (rank . 5))
                               (12293 (revision . 12293) (committer . kiyoka) (utc . 1211642182) (rank . 5))
                               (12266 (revision . 12266) (committer . kiyoka) (utc . 1211601688) (rank . 5))
                               (11607 (revision . 11607) (committer . kiyoka) (utc . 1210777124) (rank . 5))
                               (11606 (revision . 11606) (committer . kiyoka) (utc . 1210777098) (rank . 5))
                               (11603 (revision . 11603) (committer . kiyoka) (utc . 1210774733) (rank . 5))
                               (11245 (revision . 11245) (committer . kiyoka) (utc . 1210161037) (rank . 5))
                               (10961 (revision . 10961) (committer . kiyoka) (utc . 1209740697) (rank . 5))
                               (10957 (revision . 10957) (committer . kiyoka) (utc . 1209737179) (rank . 5))
                               (10149 (revision . 10149) (committer . kiyoka) (utc . 1208864713) (rank . 5))
                               (10146 (revision . 10146) (committer . kiyoka) (utc . 1208862687) (rank . 5))
                               (10084 (revision . 10084) (committer . kiyoka) (utc . 1208784928) (rank . 5))
                               (10014 (revision . 10014) (committer . kiyoka) (utc . 1208695655) (rank . 5))
                               (9977 (revision . 9977) (committer . kiyoka) (utc . 1208667401) (rank . 5))
                               (9976 (revision . 9976) (committer . kiyoka) (utc . 1208667383) (rank . 5))
                               (9952 (revision . 9952) (committer . kiyoka) (utc . 1208652380) (rank . 5))
                               (9948 (revision . 9948) (committer . kiyoka) (utc . 1208650871) (rank . 5))
                               (9924 (revision . 9924) (committer . kiyoka) (utc . 1208619757) (rank . 5))
                               (9903 (revision . 9903) (committer . kiyoka) (utc . 1208606479) (rank . 5))
                               (9585 (revision . 9585) (committer . kiyoka) (utc . 1208348091) (rank . 5))
                               (9583 (revision . 9583) (committer . kiyoka) (utc . 1208346288) (rank . 5))
                               (8962 (revision . 8962) (committer . kiyoka) (utc . 1207444336) (rank . 5))
                               (8676 (revision . 8676) (committer . kiyoka) (utc . 1207141582) (rank . 5))
                               (8607 (revision . 8607) (committer . kiyoka) (utc . 1207051192) (rank . 5))
                               (8569 (revision . 8569) (committer . kiyoka) (utc . 1206968180) (rank . 5))
                               (8342 (revision . 8342) (committer . kiyoka) (utc . 1206360044) (rank . 5))
                               (8263 (revision . 8263) (committer . kiyoka) (utc . 1206115042) (rank . 5))
                               (8254 (revision . 8254) (committer . kiyoka) (utc . 1206097354) (rank . 5))
                               (8212 (revision . 8212) (committer . kiyoka) (utc . 1206027766) (rank . 5))
                               (8211 (revision . 8211) (committer . kiyoka) (utc . 1206027177) (rank . 5))
                               (8208 (revision . 8208) (committer . kiyoka) (utc . 1206016615) (rank . 5))
                               (8205 (revision . 8205) (committer . kiyoka) (utc . 1206012635) (rank . 5))
                               (8091 (revision . 8091) (committer . kiyoka) (utc . 1205842976) (rank . 5))
                               (7924 (revision . 7924) (committer . kiyoka) (utc . 1205421544) (rank . 5))
                               (7920 (revision . 7920) (committer . kiyoka) (utc . 1205419584) (rank . 5))
                               (7919 (revision . 7919) (committer . kiyoka) (utc . 1205418381) (rank . 5))
                               (7870 (revision . 7870) (committer . kiyoka) (utc . 1205336331) (rank . 5))
                               (7811 (revision . 7811) (committer . kiyoka) (utc . 1205239814) (rank . 5)))
                              (annotation
                               ((revision . 8208) (committer . kiyoka) (utc . 1206016615) (rank . 5))
                               ((revision . 13304) (committer . kiyoka) (utc . 1212675369) (rank . 4))
                               ((revision . 13317) (committer . kiyoka) (utc . 1212711085) (rank . 3))
                               ((revision . 13317) (committer . kiyoka) (utc . 1212711085) (rank . 3))
                               ((revision . 13317) (committer . kiyoka) (utc . 1212711085) (rank . 3))
                               ((revision . 13317) (committer . kiyoka) (utc . 1212711085) (rank . 3)))
                              (text "* UnitTest用のサンプルファイル" "----" "*** start" "*** [[Entry1]]" "*** [[Entry2]]" "*** end")))
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
            '((revision . 8208) (committer . kiyoka) (utc . 1206016615) (rank . 5))
            (lambda ()
              (serialize (log-by-lineno oldtype-timeline 1))))

      (test "ago string of lineno 1 <oldtype-timeline>"
            "  (2 months ago)" 
            (lambda ()
              (get-ago (log-by-lineno oldtype-timeline 1))))
      (test "ago string of lineno 1 <oldtype-page>"
            "  (2 months ago)"
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
              (ago   . "  (2 months ago)")
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

      (test-end)

      )))
