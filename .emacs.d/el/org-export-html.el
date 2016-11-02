(require 'org)
(setq org-export-with-sub-superscripts (quote {}))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done 'time)
(add-hook 'org-mode-hook 'turn-on-font-lock)

(setq org-html-validation-link nil)
;;(setq org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"org.css\" /")
(setq org-html-creator-string "Powered by <a href=\"http://www.gnu.org/software/emacs/\">Emacs</a>")
(setq org-html-infojs-options (quote ((path . "http://orgmode.org/org-info.js") (view . "info") (toc . :with-toc) (ftoc . "0") (tdepth . "max") (sdepth . "max") (mouse . "underline") (buttons . "0") (ltoc . "nil") (up . :html-link-up) (home . :html-link-home))))
(defun my-org-inline-css-hook (exporter)
  "Insert custom inline css"
  (when (eq exporter 'html)
    (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
           (path (concat dir "style.css"))
           (homestyle (or (null dir) (null (file-exists-p path))))
           (final (if homestyle "~/.emacs.d/data/css/org-style.css" path)))
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

(setq org-export-with-email t)
(setq org-export-default-language "zh-CN")
(setq user-mail-address "kapuddi@163.com")
(setq user-full-name "Kapuddi Liu")
