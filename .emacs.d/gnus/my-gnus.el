;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;                   Gnus basic setting
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-mail-address "kapuddi@gmail.com")
(setq user-full-name "Kapuddi")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;                   Gnus NewsGroup settings
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq gnus-home-directory "~/.emacs.d/gnus/")

(gnus-add-configuration '(article
                          (vertical 1.0
                                    (summary .4 point)
                                    (article 1.0))))
;; group buffer line format
(setq gnus-group-line-format "%M%S%p%P%5y:%B%(%g%)%l %O\n")
;; summary buffer line format
(setq gnus-summary-line-format
	  ":%U%R %B %s %-100=|%3L|%-20,20n|%&user-date; \n")

(setq gnus-select-method '(nntp "news.gmane.org"))
;;(setq gnus-secondary-select-methods '(nntp "news.gnus.org"))

;;;raise speed.
(setq gnus-check-new-newsgroups 'nil)
(setq gnus-save-killed-list 'nil)
(setq gnus-read-active-file 'nil)
(setq gnus-nov-is-evil 'nil)
(setq gnus-check-bogus-newsgroups 'nil)
(setq gnus-inhibit-startup-message 't)
;;group line specification
(setq gnus-group-line-format "%M%S%10y:%B%(%g%)\n")

(setq gnus-use-cache 'passive)

(setq gnus-message-archive-group
      '((if (message-news-p)
            "nnfolder:mail.sent.news"
          "nnfolder:mail.sent.mail")))

(add-hook 'gnus-article-prepare-hook
          (lambda ()
            (setq fill-column 80)
            (gnus-article-fill-long-lines)))

(add-hook 'gnus-article-prepare-hook 'gnus-article-date-local)
