;;;-*- mode: lisp-interaction; syntax: elisp -*-;;;
;;
;; "oldtype-mode.el" is an WYSIWYG majar mode for OldType format
;;
;;   Copyright (C) 2007 Kiyoka Nishiyama
;;
;;     $Id: oldtype-mode.el 242 2008-02-02 09:58:22Z kiyoka $
;;
;; This file is part of oldtype-mode
;;
;; oldtype-mode is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; oldtype-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with OldType; see the file COPYING.
;;
;;
;;
;; How to nstall and How to use:
;;     http://oldtype.sumibi.org/show-page/oldtype-mode
;;
;;
;; ChangeLog:
;;   [0.1.1]
;;     1. Supported URL to ##(nicovideo ID) command conversion feature.  ( C-c C-c key )
;;
;;   [0.1.0]
;;     1. Added oldtype-todays-entry() function.
;; 
;;   [0.0.9]
;;     1. Bugfix: 'Converting URL to ##(amazon ASIN) command' feature generates wrong ASIN code.
;;
;;   [0.0.8]
;;     1. Fixed bug: illegal ASINCODE of amazon and VIDEOCODE of youtube use for image file creation.
;;
;;   [0.0.7]
;;     1. Fixed bug: Added The '-' character to ASINCODE of amazon and VIDEOCODE of youtube.
;;
;;   [0.0.6]
;;     1. Improved URL to [URL|TITLE] conversion feature.  ( C-c C-c key )
;;        oldtype-mode.el fetches remote title page and insert TITLE.
;;
;;   [0.0.5]
;;     1. Fixed bug: [return] key breaks japanese input method UI.
;;
;;   [0.0.4]
;;     1. Added image displaying feature for ##(amazon  asincode)  command.
;;     2. Added image displaying feature for ##(youtube videocode) command.
;;
;;   [0.0.3]
;;     1. Supported URL to ##(amazon ASIN) command conversion feature.  ( C-c C-c key )
;;
;;   [0.0.2]
;;     1. Added ##(todo),##(undo) command.
;;     2. Added oldtype-openfile( wikiname ) function.
;; 
;;   [0.0.1]
;;     1. first release
;;
;;
(defconst oldtype-version "0.1.0")

(defconst oldtype-wikiname-face 'oldtype-wikiname-face)
(defface  oldtype-wikiname-face
  '((((class color) (background light)) (:bold nil :foreground "green4" :underline t))
    (((class color) (background dark))  (:bold nil :foreground "green2" :underline t))
    (t                                  (:bold nil :underline t)))
  "Face used for wikiname."
  :group 'oldtype)
(defconst oldtype-wikiname-nofile-face 'oldtype-wikiname-nofile-face)
(defface  oldtype-wikiname-nofile-face
  '((((class color) (background light)) (:bold nil :foreground "green4" :underline nil))
    (((class color) (background dark))  (:bold nil :foreground "green2" :underline nil))
    (t                                  (:bold nil :underline nil)))
  "Face used for wikiname (nofile)."
  :group 'oldtype)
(defconst oldtype-indent1-face 'oldtype-indent1-face)
(defface  oldtype-indent1-face
  '((((class color) (background light)) (:bold nil :background "red" :underline nil))
    (((class color) (background dark))  (:bold nil :background "red" :underline nil))
    (t                                  (:bold nil :underline nil)))
  "Face used for indent pattern."
  :group 'oldtype)
(defconst oldtype-indent2-face 'oldtype-indent2-face)
(defface  oldtype-indent2-face
  '((((class color) (background light)) (:bold nil :background "light salmon" :underline nil))
    (((class color) (background dark))  (:bold nil :background "light salmon" :underline nil))
    (t                                  (:bold nil :underline nil)))
  "Face used for indent pattern."
  :group 'oldtype)
(defconst oldtype-indent3-face 'oldtype-indent3-face)
(defface  oldtype-indent3-face
  '((((class color) (background light)) (:bold nil :background "pink" :underline nil))
    (((class color) (background dark))  (:bold nil :background "pink" :underline nil))
    (t                                  (:bold nil :underline nil)))
  "Face used for indent pattern."
  :group 'oldtype)
(defconst oldtype-subject-face 'oldtype-subject-face)
(defface  oldtype-subject-face
  '((((class color) (background light)) (:bold t :underline nil))
    (((class color) (background dark))  (:bold t :underline nil))
    (t                                  (:bold t :underline nil)))
  "Face used for subject pattern."
  :group 'oldtype)
(defconst oldtype-hr-face 'oldtype-hr-face)
(defface  oldtype-hr-face
  '((((class color) (background light)) (:bold nil :background "dark slate gray" :underline nil))
    (((class color) (background dark))  (:bold nil :background "dark slate gray" :underline nil))
    (t                                  (:bold nil :underline nil)))
  "Face used for hr pattern."
  :group 'oldtype)
(defconst oldtype-pre-face 'oldtype-pre-face)
(defface  oldtype-pre-face
  '((((class color) (background light)) (:bold nil :background "deep sky blue" :underline nil))
    (((class color) (background dark))  (:bold nil :background "deep sky blue" :underline nil))
    (t                                  (:bold nil :underline nil)))
  "Face used for pre pattern."
  :group 'oldtype)
(defconst oldtype-alink-face     'link)
(defconst oldtype-code-face      'font-lock-doc-face)
(defconst oldtype-etc-face       'shadow)
(defconst oldtype-image-face     'font-lock-comment-face)

(defun oldtype-warning (format &rest args)
  (apply 'message (concat "OldType Warning: " format) args)
  (beep)
  (sleep-for 1))

(defconst oldtype-image-height-s 40)
(defconst oldtype-image-height-m 80)

(defconst oldtype-image-icon-string
  "H_")

(defconst oldtype-ext-name
  ".ot")

(defconst oldtype-allpages-wikiname
  "#AllPages.ot")

(defconst oldtype-image-prefix-list
  '("bmp"    "gif"    "jpeg"    "jpg"    "png"    "svg"    "tiff"    "tif"    "xbm"    "xpm"))

(defconst oldtype-imgurl-pattern
  (concat "\\(.+\\)\\.\\("
	  (mapconcat
	   (lambda (str) str)
	   oldtype-image-prefix-list
	   "\\|")
	  "\\)"))

(defconst oldtype-normal-wikiname-pattern "\\([\[][\[]\\)\\([^\]]+\\)\\([\]][\]]\\)")
(defconst oldtype-paren-wikiname-pattern "\\([\[(][\[(]\\)\\([^\]]+\\)\\([\]][\]]\\)")

(defcustom oldtype-insert-image-size 100
  "Image size of oldtype-insert-image command (C-c l)."
  :type  'integer
  :group 'oldtype)

(defcustom oldtype-convert-program "/usr/bin/convert"
  "The full-path of Imagemagick 'convert' program."
  :type  'string
  :group 'oldtype)

(defcustom oldtype-curl-program "/usr/bin/curl"
  "The full-path of 'curl' program."
  :type  'string
  :group 'oldtype)
 
(defcustom oldtype-work-directory "~/work/edit"
  "The working directory of OldType's content file (.ot file)."
  :type  'string
  :group 'oldtype)


;; --- utility ---
(defun assoc-ref (alist key)
  (let ((entry (assoc key alist)))
    (when entry
      (cdr entry))))


;;--- debugging message logger
(defvar oldtype-debug nil)                       ; debugging enable/disable flag.
(defun oldtype-debug-print (string)
  (if oldtype-debug
      (message string)))

(defun oldtype-insert-image (beg end image &rest args)
  "Display image on the current buffer.
Buffer string between BEG and END are replaced with IMAGE."
  (add-text-properties beg end (list 'display image
				     'intangible image
				     'invisible nil)))



;; insert image data to current-buffer.
;;  e.x.)
;;     (oldtype-insert-image-data "http://www.sumibi.org/sumibi/sumibi_picture.png")
(defun oldtype-insert-image-data (url)
  (cond 
   ((string-match "http://" url)
    (call-process oldtype-curl-program
		  nil
		  '(t nil)
		  nil
		  url))
   (t
    (insert-file-contents url))))


(defun oldtype-create-image (url &optional width height)
  (let (data pixel-width pixel-height
	     (m (string-match "http://" url)))
    (when (or (file-readable-p url) m)
      (when (not m)
	(setq url (oldtype-expand-full-path url)))
      (setq pixel-width (or width
			    ""))
      (setq pixel-height (or height
			     ""))
      (if (not (file-executable-p oldtype-convert-program))
	  (oldtype-warning "'%s' does not executable... Image can't be convert size." oldtype-convert-program)
	(progn
	  (setq data
		(with-temp-buffer
		  (let ((coding-system-for-read 'binary)
			(coding-system-for-write 'binary)
			(auto-image-file-mode nil))
		    (set-buffer-multibyte nil)
		    (oldtype-insert-image-data url)
		    ;; (message (format "1: min = %s  max = %s " (point-min) (point-max)))
		    (cond ((or (< 0 (length pixel-width))
			       (< 0 (length pixel-height)))
			   (call-process-region (point-min) (point-max)
						oldtype-convert-program
						t 
						'(t nil)
						nil
						"-" 
						"-resize"
						(format "%sx%s" pixel-width pixel-height)
						"PNG:-"))
			  (t
			   (call-process-region (point-min) (point-max)
						oldtype-convert-program
						t 
						'(t nil)
						nil
						"-" 
						"PNG:-")))
		    ;; (message (format "2: min = %s  max = %s " (point-min) (point-max)))
		    (buffer-substring-no-properties (point-min) (point-max)))))
	  (create-image data 'png 'data :ascent 'center))))))


(defun oldtype-insert-image-file (beg end attr-alist)
  "Display image on the current buffer
Buffer string between BEG and END are replaced with URL."

  (let ((image (assoc-ref oldtype-image-cache attr-alist)))
    (when (not image)
      (let (
	    (src    (assoc-ref attr-alist 'src))
	    (width  (assoc-ref attr-alist 'width))
	    (height (assoc-ref attr-alist 'height)))
	
	;; get the image data file
	(setq image (oldtype-create-image
		     src
		     width height))
	;; push to cache
	(push 
	 `(,attr-alist
	   .
	   ,image)
	 oldtype-image-cache)
	))
    ;; insert to buffer
    (oldtype-insert-image beg end image)))

;;
;; --- test code ---
;;
;;(setq  oldtype-image-cache '())
;;
;;(oldtype-insert-image-file 
;; (+ (point) 100)
;; (+ (point) 101)
;; '(
;;   (src    . "../doc/img/oldtype_logo.png")
;;   (width  . "40")
;;   (height . "40")))
;;
;;(insert (pp oldtype-image-cache))


(defun oldtype-remove-image (beg end)
  "Remove an image which is inserted between BEG and END."
  (remove-text-properties beg end '(display nil intangible nil)))

(defun oldtype-get-image (pos)
  "Get an image object which is indexed pos."
  (get-text-property pos 'display))

(defun oldtype-expand-full-path (file)
  "Expand full path from relative path."
  (concat default-directory file))

;;
;; width=10 src="abc\"
;;   -> (("width" . "10") ("src" . "abc"))
;;
;; test pattern:
;;   (assq 'src (oldtype-parse-attribute "width=10 src=\"abc\""))
;;
(defun oldtype-parse-attribute (str)
  (let ((lst (split-string str "[ ]+"))
	(oldtype-attribute-pattern "\\([a-zA-Z]+\\)=[\"]?\\([^ \"]+[/]?\\)"))
    (mapcar
     (lambda (x)
       (if (string-match oldtype-attribute-pattern x)
	   (cons
	    (intern (match-string 1 x))
	    (match-string 2 x))
	 nil))
     lst)))


;;
;; compose region with image file
;;
(defun oldtype-compose-region-with-image (beg end icon-str file &optional width height)
  ;;--- debugging message logger
  (let* ((alist `(
		  (src . ,file)))
	 (alist  (if width
		     (cons `(width . ,width) alist)
		   alist))
	 (alist  (if height
		     (cons `(height . ,height) alist)
		   alist)))
    (compose-region beg
		    end
		    icon-str)
    (oldtype-remove-image beg
			  end)
    (oldtype-insert-image-file beg
			       end
			       alist)))

;; 
;; fontification
;;
(defun oldtype-install-fontification ()
  ;;
  ;; "4873113482"
  ;;   => "http://images.amazon.com/images/P/4873113482.09.MZZZZZZZ_.jpg"
  ;;
  ;; test pattern:
  ;;   (amazon-asincode-to-url "4873113482")
  ;;
  (defun amazon-asincode-to-url (asincode)
    (if (string-match "^[a-zA-Z0-9-_]+$" asincode)
	(format "http://images.amazon.com/images/P/%s.09.MZZZZZZZ_.jpg" asincode)
      nil))

  (defun youtube-video-to-url (videocode)
    (if (string-match "^[a-zA-Z0-9-_]+$" videocode)
	(format "http://img.youtube.com/vi/%s/1.jpg" videocode)
      nil))

  (defun code-linep (pos)
    (save-excursion
      (goto-char pos)
      (eq ?! (char-after (point-at-bol)))))

  (let (
	(_indent-pattern
	 "^\\([*][*]?[*]?\\|[-][-]?[-]?\\|[#][#]?[#]?\\)[ ].*$")
	(_subject-pattern
	 "^[*][*]?[*]?[ ]\\(.*\\)$")
	(_pre-pattern
	 "^\\([ ]+\\)")
	(_hr-pattern
	 "^---[-]+$")
	(_imgurl-pattern
	 oldtype-imgurl-pattern)
	(_url-pattern
	 "http://[^\]\n\"]+")
	(_code-pattern
	 "^!.*$")
	(_wikiname-pattern
	 "\\([\[][\[]\\)\\([^\]]+\\)\\([\]][\]]\\)")
	(_image-pattern
 	 "\\(##[(]\\(img[\-]?[sm]?\\)[ ]+\\)\\([^)]+\\)\\([)]\\)")
	(_various-webservice-pattern
 	 "\\(##[(]\\(amazon\\|amazon-s\\|amazon-m\\|youtube\\|youtube-s\\|youtube-m\\)[ ]+\\)\\([^)]+\\)\\([)]\\)")
	(_simple-command-pattern
 	 "\\(##[(]\\(todo\\|done\\)[)]\\)"))

    (set (make-local-variable 'font-lock-defaults)
	 `((
	    ;; http://...
	    (,_url-pattern
	     0
	     oldtype-alink-face)

	    ;; path/of/image/file.(jpg|png|bmp ... )
	    (,_imgurl-pattern
	     0
	     oldtype-alink-face)

	    ;; ##(img URL)
	    (,_image-pattern
	     2
	     (when (not (code-linep (match-beginning 1)))
	       (let* ((beg       (match-beginning 1))
		      (image-url (match-string-no-properties 3))
		      (end       (match-end 4))
		      (height     
		       (case (intern (match-string-no-properties 2))
			 (img-s 
			  (int-to-string oldtype-image-height-s))
			 (img-m 
			  (int-to-string oldtype-image-height-m)))))
		 (oldtype-compose-region-with-image beg end oldtype-image-icon-string image-url nil height)))
	     t)

	    ;; ##(amazon asincode), ##(youtube asincode), 
	    (,_various-webservice-pattern
	     3
	     (let* ((beg       (match-beginning 1))
		    (command   (match-string-no-properties 2))
		    (value     (match-string-no-properties 3))
		    (end       (match-end 4))
		    (image-url (case (intern command)
				 ((amazon amazon-s amazon-m) 
				  (amazon-asincode-to-url value))
				 ((youtube youtube-s youtube-m)
				  (youtube-video-to-url value))
				 (t
				  "")))
		    (height
		     (case (intern command)
		       ((youtube-s amazon-s)
			(int-to-string oldtype-image-height-s))
		       ((youtube-m amazon-m)
			(int-to-string oldtype-image-height-m)))))
	       (if image-url
		   (oldtype-compose-region-with-image beg end oldtype-image-icon-string image-url nil height)))
	     t)
	    
	    ;; ##(todo), ##(done) ...
	    (,_simple-command-pattern
	     2
	     (let ((beg         (match-beginning 1))
		   (end         (match-end 1))
		   (image-url
		    (case (intern (match-string-no-properties 2))
		      (todo
		       "../img/icon.todo.png")
		      (t
		       "../img/icon.done.png"))))
	       (oldtype-compose-region-with-image beg end oldtype-image-icon-string image-url))
	     t)

	    ;; [[WikiName]] or [[URL|Name]]
	    (,_wikiname-pattern
	     2
	     (when (not (code-linep (match-beginning 2)))
	       (let*
		   ((_elem     (match-string 2))
		    (_url-pair (save-match-data 
				 (split-string _elem "|")))
		    (_url-mode (<= 2 (length _url-pair)))
		    )
		 (cond
		  ;; 1
		  ((eq 1 (length _elem))
		   (compose-region (match-beginning 1)
				   (match-end 3)
				   _elem)
		   (put-text-property (match-beginning 1)
				      (match-end 3)
				      'face 
				      (if (file-exists-p (concat (match-string 2) oldtype-ext-name))
					  oldtype-wikiname-face
					oldtype-wikiname-nofile-face)))
		  ;; over 2
		  (t
		   (compose-region (match-beginning 1)
				   (+ (match-end 1)
				      (if _url-mode
					  (+ (length (car _url-pair)) 2)
					1))
				   (car (string-to-list 
					 (if _url-mode
					     (cadr _url-pair)
					   (match-string 2)))))
		   (compose-region (- (match-end    2) 1)
				   (match-end 3)
				   (car
				    (reverse
				     (string-to-list (match-string 2)))))
		   (put-text-property (match-beginning 1)
				      (match-end 2)
				      'face 
				      (cond
				       (_url-mode
					oldtype-alink-face)
				       ((file-exists-p (concat (match-string 2) oldtype-ext-name))
					oldtype-wikiname-face)
				       (t
					oldtype-wikiname-nofile-face)))))))
	     nil)

	    ;; indent pattern like  "*** " "--- " "### "
	    (,_indent-pattern 
	     1
	     (let ((str (match-string-no-properties 1)))
	       (cond 
		((= 1 (length str))
		 oldtype-indent1-face)
		((= 2 (length str))
		 oldtype-indent2-face)
		(t
		 oldtype-indent3-face)))
	     t)

	    (,_subject-pattern
	     1
	     (let ((str (match-string-no-properties 1)))
	       oldtype-subject-face)
	     nil)

	    (,_pre-pattern 
	     0
	     oldtype-pre-face
	     t)

	    (,_hr-pattern 
	     0
	     oldtype-hr-face
	     t)

	    ;; ! ....
	    (,_code-pattern
	     0
	     oldtype-code-face
	     t)

	    )))))



(defun oldtype-mode-hookfunc-stuff ()

  ;; Remove character compositions
  (eval '(decompose-region (point-min) (point-max)))
  ;; Install fontification
  (when (and (boundp 'font-lock-keywords)
	     (symbol-value 'font-lock-keywords)
	     (not (featurep 'noweb-mode)))
    ;; This warning is not given if the `noweb-mode' package is installed.
    (oldtype-warning "`font-lock-keywords' already set when hook ran."))
  (set (make-local-variable 'oldtype-image-cache) '())

  (oldtype-install-fontification))


(defun oldtype-search-alink ()
  "Search alink or wikiname from current line."
  (let (
	(_wikiname-pattern oldtype-normal-wikiname-pattern)
	(match-list '()))

    (save-excursion
      (goto-char (point-at-bol))
      (while (re-search-forward _wikiname-pattern (point-at-eol) t)
	(let* ((start (match-beginning 2))
	       (end   (match-end 2))
	       (url   (match-string-no-properties 2)))
	  (when (string-match "^\\([^|]+\\)|" url)
	    (setq url (match-string 1 url)))
	  (push 
	   `(
	     (start . ,start)
	     (end   . ,end)
	     (url   . ,url))
	   match-list))))
    match-list))



(defun oldtype-open-allpages ()
  (defun buffer-exists-p (name)
    (member 
     name
     (mapcar
      (lambda (x)
	(buffer-name x))
      (buffer-list))))
    
  "Open href source of a tag."
  (interactive)
  (when (buffer-exists-p oldtype-allpages-wikiname)
    (kill-buffer oldtype-allpages-wikiname))
  (find-file-read-only 
   (concat (getenv "OTHOME")
	   "/edit/"
	   oldtype-allpages-wikiname)))


(defun oldtype-open-alink ()
  "Open href source of a tag."
  (interactive)
  (let ((alink-list (oldtype-search-alink))
	(found nil))
    (mapcar
     (lambda (alink-data)
       (let ((start  (assoc-ref alink-data 'start))
	     (end    (assoc-ref alink-data 'end))
	     (url    (assoc-ref alink-data 'url)))
	 (if (and (<= start (point))
		  (<= (point) end))
	     (progn
	       (setq found t)
	       (if (string-match "http://" url)
		   (browse-url url)
		 (find-file (concat url oldtype-ext-name)))))))
     alink-list)
    (if (not found)
	(newline))))


(defun oldtype-todays-entry ()
  "Open today's blog entry file."
  (interactive)

  (defun oldtype-today ()
    (let* ((oldtype-hour-offset 0)
	   (offset-second (* oldtype-hour-offset 60 60))
	   (now (current-time))
	   (high (nth 0 now))
	   (low (+ (nth 1 now) offset-second))
	   (micro (nth 2 now)))
      (setq high (+ high (/ low 65536))
	    low (% low 65536))
      (when (< low 0)
	(setq high (1- high)
	      low (+ low 65536)))
      (list high low micro)))

  (let ((wikiname 
	 (concat (getenv "USER")
		 "."
		 (format-time-string "%Y_%m_%d" (oldtype-today)))))
    (oldtype-openfile wikiname)))


;;
;; [fetch command]
;;   w3m -no-graph -halfdump -o ext_halfdump=1 -o strict_iso2022=0 -o fix_width_conv=1 URL
;;       | awk '-F<' '/title_alt/ { print $2; }' | tail -1 | awk '-F"' '{ print $2; }'
;;
(defun oldtype-fetch-html-title (url)
  (cond 
   ((string-match "http://" url)
    (with-temp-buffer
      (shell-command 
       (concat
	"w3m -no-graph -halfdump -o ext_halfdump=1 -o strict_iso2022=0 -o fix_width_conv=1 \'" url "\' |"
	"awk \'-F\<\' \'/title_alt/ { print $2; }\' |"
	"tail -1 |"
	"awk \'-F\"\' \'{ printf(\"%s\", $2); }\'")
       (current-buffer))
      (replace-string "[" "<" nil (point-min) (point-max))
      (replace-string "]" ">" nil (point-min) (point-max))
      (buffer-substring-no-properties (point-min) (point-max))))
   (t
    "No Title")))


(defun oldtype-mode-hookfunc ()

  (defun oldtype-fix-wysiwyg-object ()
    (interactive)
    (let (
	  (_wikiname-pattern oldtype-paren-wikiname-pattern)
	  (_sexp-pattern
	   "\\(##?[(]\\)\\([a-zA-Z0-9-]+[ ]*[^)]+\\)\\([)]\\)")
	  (_img-pattern oldtype-imgurl-pattern)
	  (_url_file-pattern
	   "\\(http://[^\t \n]+\\|.+html?\\)")
	  (_url_amazon-pattern
	   "\\(http://.*amazon[.]c.*\\)/\\([0-9A-Z-][0-9A-Z-][0-9A-Z-][0-9A-Z-][0-9A-Z-][0-9A-Z-][0-9A-Z-][0-9A-Z-][0-9A-Z-][0-9A-Z-]\\)[^0-9A-Z-]?\\(.*\\)")
	  (_url_amazon-pattern-part
	   "/dp/\\([0-9A-Z-][0-9A-Z-][0-9A-Z-][0-9A-Z-][0-9A-Z-][0-9A-Z-][0-9A-Z-][0-9A-Z-][0-9A-Z-][0-9A-Z-]\\)")
	  (_url_youtube-pattern
	   "\\(http://.*youtube[.]com/watch\\?v=\\)\\([0-9A-Za-z_-]+\\)\\(.*\\)")
	  (_url_nicovideo-pattern
	   "\\(http://.*nicovideo[.]jp/watch/\\)\\([0-9A-Za-z_-]+\\)"))

      (let ((cur    (point))
	    (str    (buffer-substring-no-properties (point) (point-at-eol))))
	(cond
	 ;; [[Wiki Name]]
	 ((string-match      (concat "^" _wikiname-pattern) str)
	  (re-search-forward             _wikiname-pattern  (point-at-eol) t)
	  (let ((start (match-string-no-properties 1))
		(str   (match-string-no-properties 2))
		(end   (match-string-no-properties 3)))
	    (delete-region (match-beginning 1) (match-end 3))
	    (let ((pos (point)))
	      (if (string-equal "((" start)
		  (insert "[[")
		(insert "(("))
	      (insert str)
	      (insert end)
	      (goto-char pos))))
	 ;; ##(func arg1 arg2 ...)
	 ((string-match      (concat "^" _sexp-pattern) str)
	  (re-search-forward             _sexp-pattern  (point-at-eol) t)
	  (let ((start (match-string-no-properties 1))
		(str   (match-string-no-properties 2))
		(end   (match-string-no-properties 3)))
	    (if (oldtype-get-image  (match-beginning 1))
		(progn
		  (oldtype-remove-image (match-beginning 1) (match-end 3))
		  (message "image"))
	      (message"not image"))
	    (delete-region (match-beginning 1) (match-end 3))
	    (let ((pos (point)))
	      (if (string-equal "##(" start)
		  (insert "#(")
		(insert "##("))
	      (insert str)
	      (insert end)
	      (goto-char pos))))
	 ((equal ?< (char-after (point)))
	  (if (oldtype-get-image (point))
	      (progn
		(oldtype-remove-image (point-at-bol)
				      (point-at-eol))
		(message "image"))
	    (message"not image"))
	  (delete-char 1))
	 ;; path/of/image/file.(jpg|png|bmp ... )
	 ((string-match      (concat "^" _img-pattern) str)
	  (re-search-forward             _img-pattern (point-at-eol) t)
	  (goto-char (match-end 2))
	  (insert ")")
	  (goto-char (match-beginning 1))
	  (let ((img-str "##(img "))
	    (insert img-str)
	    (backward-char (string-bytes img-str))))
	 ;; http://amazon/?/ASIN/ASINCODE/... amazon-command
	 ((string-match      (concat "^" _url_amazon-pattern) str)
	  (re-search-forward             _url_amazon-pattern (point-at-eol) t)
	  (let* ((asin  (match-string 2))
		 (url   (match-string 0))
		 (s     (match-beginning 1))
		 (e     (match-end 3))
		 (title (oldtype-fetch-html-title url)))
	    (when (string-match    _url_amazon-pattern-part  str)
	      (setq asin (match-string 1 str)))
	    (delete-region s e)
	    (goto-char s)
	    (insert (format "##(amazon %s)  %s" asin title))))
	 ;; http://www.youtube.com/watch ...  youtube-command
	 ((string-match      (concat "^" _url_youtube-pattern) str)
	  (re-search-forward             _url_youtube-pattern (point-at-eol) t)
	  (let* ((video (match-string 2))
		 (url   (match-string 0))
		 (s     (match-beginning 1))
		 (e     (match-end 3))
		 (title (oldtype-fetch-html-title url)))
	    (delete-region s e)
	    (goto-char s)
	    (insert (format "##(youtube %s)  %s" video title))))
	 ;; http://www.nicovideo.jp/watch ...  nicovideo-command
	 ((string-match      (concat "^" _url_nicovideo-pattern) str)
	  (re-search-forward             _url_nicovideo-pattern (point-at-eol) t)
	  (let* ((video (match-string 2))
		 (url   (match-string 0))
		 (s     (match-beginning 1))
		 (e     (match-end 2)))
	    (delete-region s e)
	    (goto-char s)
	    (insert (format "##(nicovideo %s)" video))))
	 ;; http://host/path/of/contents... anchor-keyword
	 ((string-match      (concat "^" _url_file-pattern) str)
	  (re-search-forward             _url_file-pattern (point-at-eol) t)
	  (let* ((url   (match-string 1))
		 (s     (match-beginning 1))
		 (e     (match-end 1))
		 (title (oldtype-fetch-html-title url)))
	    (delete-region s e)
	    (goto-char s)
	    (insert (format "[[%s|%s]]" url title))))
	 (t
	  (message "OldType: Please move cursor to [[URL|Name]]  or [[WikiName]] *.png  or  ##(... )  keywword.' "))))))

  (defun oldtype-insert-images ()
    "Insert img tags recursivly from current directory."
    (interactive)
    
    (defun search-image-files ()
      "search image files."
      (with-temp-buffer
	(shell-command (mapconcat
			(lambda (x)
			  (format "find ../img -follow -iname '*.%s'; " x))
			oldtype-image-prefix-list
			"")
		       (current-buffer))
	(split-string
	 (buffer-substring-no-properties (point-min) (point-max)))))
    
    (defun prefix-check (line)
      (not
       (notany
	(lambda (_prefix)
	  (string-match (concat "\\." _prefix "$") line))
	oldtype-image-prefix-list)))
    
    (mapcar
     (lambda (x)
       (when (prefix-check x)
	 (insert
	  (format "##(img %s)" x oldtype-insert-image-size))))
     (search-image-files)))


  (defun oldtype-grep ()
    "grep keyword from current directory."
    (interactive)
    
    (with-temp-buffer
      (let ((command
	     (read-from-minibuffer "keyword: ")))
	(grep (concat "grep -nH -C 3 " command " * ")))))
  

  (oldtype-mode-hookfunc-stuff)
  
  ;; Bind Return/Enter key.
  (local-set-key "\C-c\C-c" 'oldtype-fix-wysiwyg-object)
  (local-set-key "\C-c,"    'oldtype-fix-wysiwyg-object)
  (local-set-key "\C-cl"    'oldtype-insert-images)
  (local-set-key "\C-m"     'oldtype-open-alink)
  (local-set-key "\C-ca"    'oldtype-open-allpages)
  (local-set-key "\C-c/"    'oldtype-grep)
  (setq mode-name "OldType"))


;;;###autoload
(define-derived-mode oldtype-mode text-mode "OldType"
  "Major mode for editing OldType documents.

Do \\[describe-variable] oldtype- SPC to see available variables.
Do \\[describe-key] on the following bindings to discover what they do.
\\{oldtype-mode-map}"

  (oldtype-mode-hookfunc))


;;
;; When you eval sexp as follows, open oldtype contents file.
;;
;; (oldtype-openfile "index")[C-x C-e]
;;
(defun oldtype-openfile (wikiname &optional lineno)
  "open oldtype contents file."
  (find-file (concat
	      (if (string-match "/$" oldtype-work-directory)
		  oldtype-work-directory		  
		(concat oldtype-work-directory "/"))
	      wikiname oldtype-ext-name))
  (when lineno
    (goto-line lineno (concat wikiname ".ot"))))


(provide 'oldtype)
;; oldtype-mode.el ends here
