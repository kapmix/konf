;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                                                                            ;;
;; Qiang's .emacs initialization file  ; -*- mode: emacs-lisp -*-             ;;
;;                          customized by qiang                               ;;
;;                                          2005.12.29                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package-initialize)

;;(setq package-archives
;;      '(("gnu"          . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;        ("melpa"        . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
;;        ("melpa-stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
;;        ("org"          . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
;;        ("marmalade"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/marmalade/")))

(setq package-archives '(("gnu"   . "http://elpa.zilongshanren.com/gnu/")
                         ("melpa" . "http://elpa.zilongshanren.com/melpa/")))

(setq default-directory "~/.emacs.d")
(setq default-elisp-directory (concat default-directory "/el"))
(setq default-data-directory (concat default-directory "/data"))
(setq default-tmp-directory (concat default-directory "/tmp"))

(setq load-path (cons default-elisp-directory load-path))
(setq load-path (cons "/usr/share/emacs/site-lisp" load-path))
(setq load-path (cons "~/.local/share/emacs/site-lisp" load-path))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 0. About System
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For windows
(if (eq system-type 'windows-nt)
    (progn
	  (setq explicit-shell-args '("--login" "--init-file" "~/.bash_profile" "-i"))
      (setq tools-dir "D:/Tools")
      (setq cygwin-dir (concat tools-dir "/cygwin"))
      (setenv "PATH" 
      	      (concat cygwin-dir "/usr/local/bin;" cygwin-dir "/usr/bin;" cygwin-dir "/bin;" (getenv "PATH")))
      (setenv "INFOPATH" 
      	      (concat tools-dir "/Emacs/info;" cygwin-dir "/usr/share/info;" (getenv "INFOPATH")))
      ;(setq exec-path (append `(,(concat cygwin-dir "/bin") ,(concat cygwin-dir "/usr/local/bin")) exec-path))
	  (setq exec-path (append exec-path '("D:/Tools/cygwin/bin/")))
      ;; Adding cygwin bash shell
      (setq shell-file-name (concat cygwin-dir "/bin/bash"))
      (setenv "SHELL" shell-file-name)
      (setq explicit-shell-file-name shell-file-name)
      (setq ediff-shell shell-file-name)
      (setq explicit-shell-args '("--login" "-i"))
      (setenv "include" (concat cygwin-dir "/usr/include"))

      (require 'w32-browser)
      (require 'cygwin-mount)
      (cygwin-mount-activate)
      (defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
        "Use cygwin's /dev/null as the null-device."
        (let ((null-device "/dev/null"))
          ad-do-it))
      (ad-activate 'grep-compute-defaults)

	  (setq w32-get-true-file-attributes nil)


	  ;; Setting about network printer in Windows
	  ;; in windows terminal, run command like
	  ;; 	net use LPT3: \\127.0.0.1\PrinterL19 /persistent:yes
	  ;; set the sharing name of the network printer in its properties: PrinterL19
	  (setq printer-name "LPT3:")

	  (setenv "GS_LIB" "d:/Tools/gs/gs9.20/lib")
	  (setq ps-lpr-command "d:/Tools/gs/gs9.20/bin/bin/gswin64c.exe")
	  (setq ps-lpr-switches '("-q" "-dNOPAUSE" "-dBATCH" "-sDEVICE=mswinpr2"))
	  (setq intl-fonts-dir "/usr/share/fonts/intlfonts-1.2.1")

      (set-selection-coding-system 'utf-8-unix)
      (set-clipboard-coding-system 'cn-gb-2312)
      (setq auto-coding-alist
            (append auto-coding-alist '(("\\.txt\\'" . utf-8-unix))))
      (setq buffer-file-coding-system 'utf-8-unix)
      (setq coding-system-for-write 'utf-8-unix)
	  (setq coding-system-for-read 'utf-8-unix)
      ))

;; For OSX
(if (eq system-type 'darwin)
    (progn
      (setenv "PATH" 
			  (concat "/usr/local/bin:" (getenv "PATH")))
      (setq exec-path
			(append exec-path '("/usr/local/bin")))
	  (require 'exec-path-from-shell)
	  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "USERNAME" "EMAIL"))
	  (exec-path-from-shell-initialize)
      (require 'reveal-in-osx-finder)
      ))

;; For GNU/Linux
(if (eq system-type 'gnu/linux)
    (progn
      ;; Enable copy content from emacs to clipboard
      ;; in order that other programs can get it.
      ;;(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
      (setq select-enable-clipboard t)
      (set-selection-coding-system 'utf-8)
      (set-clipboard-coding-system 'utf-8)

      (setq auto-coding-alist
			(append auto-coding-alist '(("\\.txt\\'" . utf-8))))
      (setq buffer-file-coding-system 'utf-8)
      (setq coding-system-for-write 'utf-8)
      ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 1. My General configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "sos")
(require 'tabbar)
(tabbar-mode 1)
(setq tabbar-cycling-scope (quote tabs))
(require 'highlight-parentheses)

(require 'ibuffer)
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(setq session-save-file (concat default-tmp-directory "/session"))

;; undo-tree
(global-undo-tree-mode)

(require 'recentf)
(setq recentf-max-saved-items 100
      recentf-max-menu-items 15)
(recentf-mode 1)
(setq recentf-save-file (concat default-tmp-directory "/recentf"))

;; Color theme
;;(when window-system
;;  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;;  ;;(add-to-list 'load-path "~/.emacs.d/themes")
;;  (load-theme 'yoshi t))




(defun gb2312 ()
  "Set coding system to UTF8..."
  (interactive)
  (set-selection-coding-system 'cn-gb-2312)
  (set-clipboard-coding-system 'cn-gb-2312)
  (setq auto-coding-alist
		(append auto-coding-alist '(("\\.txt\\'" . cn-gb-2312))))
  (setq buffer-file-coding-system 'cn-gb-2312)
  (setq coding-system-for-write 'cn-gb-2312))

(defun utf8 ()
  "Set coding system to UTF8..."
  (interactive)
  (set-selection-coding-system 'utf-8)
  (set-clipboard-coding-system 'gb2312)

  (setq auto-coding-alist
		(append auto-coding-alist '(("\\.txt\\'" . utf-8))))
  (setq buffer-file-coding-system 'utf-8)
  (setq coding-system-for-write 'utf-8))

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
		((looking-at "\\s\)") (forward-char 1) (backward-list 1))
		(t (self-insert-command (or arg 1)))))

(defun comment-current-line ()
  (interactive)
  (comment-region (line-beginning-position) (line-end-position)))

(defun google-region
    (beg end)
  "Google the selected region."
  (interactive "r")
  (if (eq system-type 'windows-nt)
      (setq browse-url-browser-function 'browse-url-generic
			browse-url-generic-program
			;;"C:/Documents and Settings/qiang/Local Settings/Application Data/Google/Chrome/Application/GChrome.exe"))
			"C:/Program Files/Internet Explorer/iexplore.exe"))
  (browse-url (concat "http://www.google.com/search?hl=zh-CN&q="
					  (buffer-substring beg end))))

(defun baidu-region
    (beg end)
  "Google the selected region."
  (interactive "r")
  (if (eq system-type 'windows-nt)
      (setq browse-url-browser-function 'browse-url-generic
			browse-url-generic-program
			;;"C:/Documents and Settings/qiang/Local Settings/Application Data/Google/Chrome/Application/GChrome.exe"))
			"C:/Program Files/Internet Explorer/iexplore.exe"))
  (browse-url (concat "http://www.baidu.com/s?wd="
					  (buffer-substring beg end))))

(defun open-fe()
  "Open current directory in File Explorer."
  (interactive)
  (let ((currdir (file-name-directory (or load-file-name buffer-file-name dired-directory))))
    (pcase system-type
      (`windows-nt (w32explore currdir))
      (`darwin (reveal-in-osx-finder))
      (`gnu/linux (shell-command "nautilus" currdir))
      )))

;; Print to pdf
(require 'ps-print)
(setq ps-printer-name t)
(setq ps-top-margin 100)
(setq ps-bottom-margin 20)
(setq ps-font-info-database
      (append
       '((Consolas
          (fonts (normal      . "Consolas")
                 (bold        . "Consolas-Bold")
                 (italic      . "Consolas-Italic")
                 (bold-italic . "Consolas-Bold-Italic"))
          (size           . 11.0)
          (line-height    . 13.0)
          (space-width    . 6.04688)
          (avg-char-width . 6.04688)))
       ps-font-info-database))
(setq ps-font-family 'Consolas)
(setq ps-font-size 11)
(setq ps-paper-type 'a4)
(setq ps-print-color-p t)

;; Page layout: Header [file-name            ]
;;              Footer [2011-12-05        n/m]
;; Header
(setq ps-print-header nil)
(setq ps-header-lines 1)
(setq ps-header-font-size 9)
(setq ps-header-title-font-size 9)
(setq ps-header-font-family 'Consolas)
(setq ps-right-header '(ps-time-stamp-yyyy-mm-dd))
(setq ps-print-header-frame t)        ; no box top

;; Footer
(setq ps-print-footer t)
(setq ps-footer-lines 1)
(setq ps-footer-font-size 9)
(setq ps-footer-font-family 'Consolas)
(setq ps-left-footer '(ps-time-stamp-yyyy-mm-dd))
(setq ps-right-footer (list "/pagenumberstring load"))
(setq ps-footer-offset 20)
(setq ps-footer-line-pad .50)
(setq ps-print-footer-frame nil)
(setq ps-footer-frame-alist (quote ((fore-color . 0.0)
									(back-color . 1.0)
									(border-width . 0.0)
									(border-color . 0.0)
									(shadow-color . 1.0))))



(defun pdf-print-buffer-with-faces (&optional filename)
  "Print file in the current buffer as pdf, including font, color, and
underline information.  This command works only if you are using a window system,
so it has a way to determine color values.

C-u COMMAND prompts user where to save the Postscript file (which is then
converted to PDF at the same location."
  (interactive (list (if current-prefix-arg
						 (ps-print-preprint 4)
					   (concat (file-name-sans-extension (buffer-file-name))
							   ".ps"))))
  (ps-print-with-faces (point-min) (point-max) filename)
  (shell-command (concat "ps2pdf " filename))
  (delete-file filename)
  (message "Deleted %s" filename)
  (message "Wrote %s" (concat (file-name-sans-extension filename) ".pdf")))

(auto-image-file-mode 1)
(column-number-mode 1)
(delete-selection-mode 1)

(display-time)
(electric-pair-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(ido-mode 1)
(setq ido-save-directory-list-file (concat default-tmp-directory "/ido.last"))
(mouse-avoidance-mode 'animate)
(put 'upcase-region 'disabled nil)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

(global-visual-line-mode 1)

(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

(transient-mark-mode 1)
(which-function-mode 1)
(delete-selection-mode 1)

(setq-default cursor-type 'bar)
(setq-default line-spacing 4)

(setq auto-save-interval 5)
(setq bookmark-default-file (concat default-data-directory "/emacs.bmk"))
(setq bookmark-save-flag 1)
(setq confirm-kill-emacs 'yes-or-no-p)
;;(setq major-mode 'text-mode)


(setq display-time t)
(setq display-time-24hr-format t)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-interval 10)
(setq frame-title-format "%b")
(setq global-font-lock-mode t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq ispell-extra-args '("-l" "en"))
(setq line-move-visual nil)
(setq-default next-line-add-newlines nil)
(setq require-final-newline nil)
(setq track-eol t)
(setq transient-mark-mode t)
(setq truncate-lines t)
(setq uniquify-buffer-name-style 'forward)
(setq visible-bell t)


(setq make-backup-files t)
(setq version-control t)
(setq kept-old-versions 2)
(setq kept-new-versions 5)
(setq delete-old-versions t)
(setq backup-directory-alist '((""."~/.backup/emacsbackup")))


(if window-system
    (progn
      ;; enable wheelmouse support by default
      (mwheel-install)
      (if (eq system-type 'windows-nt)
		  (progn
			(set-frame-font "Consolas:pixelsize=14")
			(set-fontset-font (frame-parameter nil 'font)
							  'han '("Microsoft YaHei" . "unicode-bmp"))
			(set-fontset-font (frame-parameter nil 'font)
							  'cjk-misc '("Microsoft Yahei" . "unicode-bmp"))
			(set-fontset-font (frame-parameter nil 'font)
							  'bopomofo '("Microsoft Yahei" . "unicode-bmp"))
			(set-fontset-font (frame-parameter nil 'font)
							  'gb18030 '("Microsoft Yahei". "unicode-bmp"))
			(setq face-font-rescale-alist '(("Microsoft Yahei" . 1.2) ("WenQuanYi Zen Hei" . 1.2)))))
      (if (eq system-type 'darwin)
		  (progn
			(set-frame-font "Monaco:pixelsize=15")
			(dolist (charset '(han kana symbol cjk-misc bopomofo))
			  (set-fontset-font (frame-parameter nil 'font)
								charset
								(font-spec :family "Hiragino Sans GB" :size 18)
								))))
      ))

;; chinese fonts
(require 'chinese-fonts-setup)
(setq cfs-profiles-directory (concat default-data-directory "/chinese-fonts-setup"))
(chinese-fonts-setup-enable)
(cfs-set-spacemacs-fallback-fonts)

;; dired
(require 'dired-x)
(setq dired-omit-files "^\\\\.?#\\\\|^\\\\.$\\\\|^\\\\.\\\\.$\\\\|^\\\\.\\\\|^~\\\\|^CVS$")
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-recursive-deletes t)
(setq cvs-dired-use-hook t)
(add-hook 'dired-load-hook
		  (lambda ()
			(require 'dired-x)
			;; Set dired-x global variables here.  For example:
			;; (setq dired-guess-shell-gnutar "gtar")
			;; (setq dired-x-hands-off-my-keys nil)
			(setq dired-omit-files
				  (concat dired-omit-files "\\|^\\..+$"))
			))
(add-hook 'dired-mode-hook
		  (lambda ()
			;; Set dired-x buffer-local variables here.  For example:
			;;	    (dired-omit-mode 1)
			))

(setq dired-mark-keys "\C-o")
;;; Autoload `dired-jump' and `dired-jump-other-window'.
;;; We autoload from FILE dired.el.  This will then load dired-x.el
;;; and hence define `dired-jump' and `dired-jump-other-window'.

(autoload (quote dired-jump) "dired" "\
     Jump to Dired buffer corresponding to current buffer.
     If in a file, Dired the current directory and move to file's line.
     If in Dired already, pop up a level and goto old directory's line.
     In case the proper Dired file line cannot be found, refresh the Dired
     buffer and try again." t nil)

(autoload (quote dired-jump-other-window) "dired" "\
     Like \\[dired-jump] (dired-jump) but in other window." t nil)

(setq dired-guess-shell-alist-user
      (list
       (list "\\.rm$" "mplayer -fs")
       (list "\\.rmvb$" "mplayer -fs")
       (list "\\.avi$" "mplayer -fs")
       (list "\\.asf$" "mplayer -fs")
       (list "\\.wmv$" "mplayer -fs")
       (list "\\.chm$" "xchm")
       (list "\\.htm$" "w3m")
       (list "\\.html$" "w3m")
       (list "\\.mpg$" "mplayer -fs")
       (list "\\.pdf$" "evince")
       (list "\\.tar.bz2$" "tar jxvf")
       (list "\\.tar.gz$" "tar zxvf")
       (list "\\.rar$" "rar e")
       (list "\\.p[bgpn]m$" "display")
       (list "\\.gif$" "display")		; view gif pictures
       (list "\\.tif$" "display")
       (list "\\.png$" "display")		; xloadimage 4.1 doesn't grok PNG
       (list "\\.jpe?g$" "display")       
       )
      )

(require 'auto-complete-config)
(ac-config-default)
(setq ac-comphist-file (concat default-tmp-directory "/ac-comphist.dat"))

(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)


(setq my-desktop-dirname (concat default-tmp-directory "/desktop/"))
(if (not (file-exists-p my-desktop-dirname))
    (make-directory my-desktop-dirname))
(setq desktop-dirname my-desktop-dirname)
(desktop-save-mode 1)
(setq desktop-load-locked-desktop t)
(add-to-list 'desktop-path my-desktop-dirname)

(server-force-delete)
(server-start)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 1. My Configuration about Documentation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; document
(setq load-path (cons (concat default-elisp-directory "/doc") load-path))

;; Org
(require 'org)
(setq org-export-with-sub-superscripts (quote {}))
(setq org-log-done 'time)
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook (lambda () (auto-fill-mode -1)))
(setq org-export-with-tags nil)
(setq org-startup-folded (quote content))
(setq org-todo-keywords
	  '((sequence "TODO"
				  "SCHEDULED"
				  "DOING"
				  "SUSPEND"
				  "DONE"
				  "CANCELLED"
				  "CLOSED"
				  )))

(setq org-todo-keyword-faces
	  '(("TODO" :background "#64b4ff" :foreground "black" :weight bold :box (:line-width 2 :style released-button))
		("SCHEDULED" :background "orange" :foreground "black" :weight bold :box (:line-width 2 :style released-button))
		("DOING" :background "red1" :foreground "black" :weight bold :box (:line-width 2 :style released-button))
		("SUSPEND" :background "orange" :foreground "black" :weight bold :box (:line-width 2 :style released-button))
		("DONE" :background "forest green" :foreground "black" :weight bold :box (:line-width 2 :style released-button))
		("CANCELLED" :background "gray" :foreground "black" :weight bold :box (:line-width 2 :style released-button))
		("CLOSED" :background "forest green" :foreground "black" :weight bold :box (:line-width 2 :style released-button))
		))

(add-to-list 'org-latex-packages-alist '("UTF8" "ctex"))

;; org-mode color
;; [[color:blue][test this out]]
(org-add-link-type
 "color"
 (lambda (path)
   (message (concat "color "
                    (progn (add-text-properties
                            0 (length path)
                            (list 'face `((t (:foreground ,path))))
                            path) path))))
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "<span style=\"color:%s;\">%s</span>" path desc))
    ((eq format 'latex)
     (format "{\\color{%s}%s}" path desc)))))

(defun gtd ()
  (interactive)
  (find-file "~/.Gtd/MyGtd.org"))

(if (eq system-type 'windows-nt)
	  (defcustom my-org-base-dir "d:/doc/mydoc/org"
		"The storage directory for diary."
		:type 'string
		:group '02_doc)
	(defcustom my-org-base-dir "~/doc/mydoc/org"
	  "The storage directory for diary."
	  :type 'string
	  :group '02_doc))
	
(defun open-day ()
  "Open today. Draw plan..."
  (interactive)
  (setq my-today-org (concat my-org-base-dir (format-time-string "/%Y%m%d.org")))
  (if (not (file-exists-p my-org-base-dir))
      (make-directory my-org-base-dir t))
  (find-file my-today-org))

(defun open-next-day ()
  "Open tomorrow. Draw plan..."
  (interactive)
  (setq my-today-org (concat my-org-base-dir (format-time-string "/%Y%m%d.org"(time-add (current-time) (days-to-time 1)))))
  (if (not (file-exists-p my-org-base-dir))
      (make-directory my-org-base-dir t))
  (find-file my-today-org))

(defun open-week ()
  "Open this week. Draw plan..."
  (interactive)
  (defvar my-week-org (concat my-org-base-dir (format-time-string "/%Y-w%W.org")))
  (if (not (file-exists-p my-org-base-dir))
      (make-directory my-org-base-dir t))
  (find-file my-week-org))

(defun open-month ()
  "Open this month. Draw plan..."
  (interactive)
  (defvar my-month-org (concat my-org-base-dir (format-time-string "/%Y%m.org")))
  (if (not (file-exists-p my-org-base-dir))
      (make-directory my-org-base-dir t))
  (find-file my-month-org))

(defun open-year ()
  "Open this year. Draw plan..."
  (interactive)
  (defvar my-year-org (concat my-org-base-dir (format-time-string "/%Y.org")))
  (if (not (file-exists-p my-org-base-dir))
      (make-directory my-org-base-dir t))
  (find-file my-year-org))

(open-day)

;; Work Diary
(if (eq system-type 'windows-nt)
	(progn
	  (setq org-publish-project-alist
			'(
			  ("Guide"
			   :base-directory "d:/doc/mydoc/org/note/work/guide"
			   :base-extension "org"
			   ;;:publishing-directory "/ftp:qiang@109.105.20.112:/home/qiang/"
			   :publishing-directory "Z:/TTL/Qiang/doc/website"
			   :publishing-function org-html-publish-to-html
			   :exclude "Airmsg*"   ;; regexp
			   :headline-levels 3
			   :recursive t
			   :html-preamble t)
			  ("all" :components ("Guide")))))
  (progn
	(setq org-publish-project-alist
		  '(
			("org-notes"
			 :base-directory "~/doc/mydoc/org/note/"
			 :base-extension "org"
			 :publishing-directory "/var/www/"
			 :recursive t
			 :publishing-function org-html-publish-to-html
			 :headline-levels 4             ; Just the default for this project.
			 :auto-preamble t
			 )
			("org-static"
			 :base-directory "~/doc/mydoc/org/note/"
			 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
			 :publishing-directory "/var/www/"
			 :recursive t
			 :publishing-function org-publish-attachment
			 )
			("all" :components ("org-notes" "org-static"))
			))))

(setq org-html-validation-link nil)
(setq org-html-creator-string "Powered by <a href=\"http://www.gnu.org/software/emacs/\">Emacs</a>")
;;(setq org-html-infojs-options (quote ((path . "http://orgmode.org/org-info.js") (view . "info") (toc . :with-toc) (ftoc . "0") (tdepth . "max") (sdepth . "max") (mouse . "underline") (buttons . "0") (ltoc . "nil") (up . :html-link-up) (home . :html-link-home))))
(defun my-org-inline-css-hook (exporter)
  "Insert custom inline css"
  (when (eq exporter 'html)
    (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
           (path (concat dir "style.css"))
           (homestyle (or (null dir) (null (file-exists-p path))))
           (final (if homestyle (concat default-data-directory "/css/org-style.css") path)))
      (setq org-html-head-include-default-style nil)
      (setq org-html-head (concat
                           "<style type=\"text/css\">\n"
                           "<!--/*--><![CDATA[/*><!--*/\n"
                           (with-temp-buffer
                             (insert-file-contents final)
                             (buffer-string))
                           "/*]]>*/-->\n"
                           "</style>\n")))))
(eval-after-load 'ox
  '(progn
     (add-hook 'org-export-before-processing-hook 'my-org-inline-css-hook)))

(setq org-export-with-email t)
(setq org-export-default-language "zh-CN")
(setq user-mail-address (getenv "EMAIL"))
(setq user-full-name (getenv "USERNAME"))

(add-to-list 'ac-modes 'org-mode)

;; latex
;;(while nil
;;  ((load "auctex.el" nil t t)
;;   (load "preview-latex.el" nil t t)
;;   (load "doc-view.el")
;;   (setq TeX-auto-save t)
;;   (setq TeX-parse-self t)
;;   (setq-default TeX-master nil)
;;   (setq auto-mode-alist
;;	 (cons '("\\.\\(tex\\|latex\\)\\'" . latex-mode)
;;	       auto-mode-alist))
;;   (add-to-list 'auto-mode-alist '("\\.mp$" . metapost-mode))
;;   (add-hook 'latex-mode-common-hook 'highlight-parentheses-mode)
;;   (add-hook 'metapost-mode-common-hook 'highlight-parentheses-mode)
;;   ))


(require 'remember)
(setq-default remember-data-file (concat default-directory "/notes"))

;; Calender/Diary
(setq diary-file (concat default-data-directory "/diary"))
(setq calendar-latitude 39.9599)
(setq calendar-longitude 116.4543)
(setq calendar-date-style 'iso)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 2. My Configuration about Development
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load up abbrevs for these modes
(add-hook 'c-mode-hook 'hs-minor-mode)

(require 'sh-script)

;; C, C++, java
(require 'cc-mode)
(defun my-embrace-mode-auto-pair ()
  (interactive)
  (make-local-variable 'skeleton-pair-alist) 
  (setq skeleton-pair-alist  '(
							   (?\" _"\"")
							   (?\' _"\'")
							   (?\( _")") 
							   (?\[ _"]") 
							   (?{ \n > _ \n ?} >)))
  (setq skeleton-pair t)
  (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "\'") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe))
(add-hook 'c-mode-hook 'my-embrace-mode-auto-pair)
(add-hook 'c++-mode-hook 'my-embrace-mode-auto-pair)
(add-hook 'java-mode-hook 'my-embrace-mode-auto-pair)
(add-hook 'cperl-mode-hook 'my-embrace-mode-auto-pair)
(add-hook 'latex-mode-hook 'my-embrace-mode-auto-pair)
;;(add-hook 'web-mode-hook 'my-embrace-mode-auto-pair)

(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
(add-hook 'cperl-mode-hook 'my-cperl-mode-hook t)
(defun my-cperl-mode-hook ()
  (setq cperl-indent-level 4)
  (setq cperl-continued-statement-offset 0)
  (setq cperl-extra-newline-before-brace t)
  (set-face-background 'cperl-array-face "wheat")
  (set-face-foreground 'cperl-array-face "blue")
  (set-face-background 'cperl-hash-face "wheat")
  (set-face-foreground 'cperl-hash-face "blue"))


(setq tags-file-name "./TAGS")
(setq gdb-many-windows t)
(setq gdb-show-main t)
(setq c-default-style "linux")
(setq-default c-basic-offset 4)
(setq-default tab-width 4)
(setq-default indent-tabs-mode t)
(setq comment-column 40)

(font-lock-add-keywords 'c-mode '(("\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*(" 1 font-lock-function-name-face)) t)

(add-hook 'c-mode-common-hook 'highlight-parentheses-mode)
(add-hook 'c++-mode-common-hook 'highlight-parentheses-mode)
(add-hook 'java-mode-common-hook 'highlight-parentheses-mode)
(add-hook 'perl-mode-common-hook 'highlight-parentheses-mode)

(require 'php-mode)
(require 'htmlize)

;; Python
(require 'jedi-direx)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(setq jedi:setup-keys t)
(add-hook 'jedi-mode-hook 'jedi-direx:setup)

;; GNU global
(setq gtags-suggested-key-mapping t)
(require 'gtags)
(add-hook 'c-mode-common-hook 'gtags-mode)
(add-hook 'c++-mode-common-hook 'gtags-mode)
(add-hook 'java-mode-common-hook 'gtags-mode)
(add-hook 'gtags-select-mode-hook
          '(lambda ()
             (setq hl-line-face 'underline)
             (hl-line-mode 1)))
(define-key gtags-select-mode-map "RET" 'gtags-select-tag)


(setq default-frame-alist '((vertical-scroll-bars . nil)
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (fullscreen . nil)))
(blink-cursor-mode -1)

(setq magit-git-executable "/usr/bin/git")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 3. My Configuration about Key binding
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key global-map [(control tab)] 'tabbar-forward-tab)
(define-key org-mode-map [(control tab)] 'tabbar-forward-tab)
(global-set-key "%" 'match-paren)
(global-set-key (kbd "<C-S-f5>") 'hs-show-all)
(global-set-key (kbd "<C-S-f6>") 'hs-hide-all)
(global-set-key (kbd "<C-f10>") 'linum-mode)
(global-set-key (kbd "<C-f11>") 'dictionary-lookup-definition)
(global-set-key (kbd "<C-f12>") 'ido-switch-buffer)
(global-set-key (kbd "<C-f5>") 'hs-show-block)
(global-set-key (kbd "<C-f6>") 'hs-hide-block)
(global-set-key (kbd "<C-f8>") 'compile)
(global-set-key (kbd "<C-f9>") 'grep)
(global-set-key (kbd "<M-f8>") 'uncomment-region)
(global-set-key (kbd "<S-f12>") 'calendar)
(global-set-key (kbd "<s-f9>") 'ediff)
(global-set-key (kbd "C-'") 'other-window)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-+") 'text-scale-increase)
;;(global-set-key (kbd "C-,") 'gdb)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-;") 'comment-current-line)
(global-set-key (kbd "C-?") 'man)
(global-set-key (kbd "C-\\") 'set-mark-command)
(global-set-key (kbd "C-`") 'shell-command)
(global-set-key (kbd "C-c ?")  (lambda () (interactive) (manual-entry (current-word))))
(global-set-key (kbd "C-c C-k") 'kill-some-buffers)
(define-key org-mode-map (kbd "C-x C-p") 'org-html-export-to-html)
(global-set-key (kbd "C-c C-t") 'clone-indirect-buffer)
(global-set-key (kbd "C-c M-.") 'gtags-find-rtag)
(global-set-key (kbd "C-c t") '(lambda () (interactive) (insert (format-time-string "/* Needed modified in future. */"))))
(define-key org-mode-map (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x g") 'google-region)
(global-set-key (kbd "C-x t") '(lambda () (interactive) (insert (format-time-string "//blabla_%Y.%m.%d_"))))
(global-set-key (kbd "M-,") 'gtags-pop-stack)
(global-set-key (kbd "M-.") 'gtags-find-tag)
(global-set-key (kbd "M-n") 'tabbar-forward-group)
(global-set-key (kbd "M-p") 'tabbar-backward-group)
(global-set-key (kbd "M-s") 'query-replace)
(global-set-key [(meta ?/)] 'hippie-expand)
(global-set-key [M-f11] 'shrink-window)
(global-set-key [M-f12] 'enlarge-window)
(global-set-key [delete] 'delete-char)
(global-set-key [f10] (lambda () (interactive) (recenter 2)))
(global-set-key [f11] 'open-fe)
(global-set-key [f12] 'ace-jump-mode)
(global-set-key [f1] 'ibuffer)
(global-set-key [f2] 'find-name-dired)
(global-set-key [f3] 'ido-find-file)
(global-set-key [f4] 'save-buffer)
(global-set-key [f5] 'revert-buffer)
(global-set-key [f6] 'calendar)
(define-key org-mode-map [f6] 'outline-toggle-children)
(global-set-key [f7] 'recentf-open-files)
(global-set-key [f8] 'grep)
(global-set-key [f9] 'goto-line)
(global-set-key [kp-delete] 'delete-char)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 5. Gnus
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(add-to-list 'load-path "~/.emacs.d/gnus")
;;(require 'my-gnus)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cfs--current-profile "profile3" t)
 '(cfs--profiles-steps (quote (("profile3" . 3) ("profile1" . 5))) t)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil)
 '(package-selected-packages
   (quote
	(jedi-direx jedi chinese-fonts-setup org which-key undo-tree magit graphviz-dot-mode ggtags auto-complete ace-jump-mode)))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "gray60" :slant italic)))))
