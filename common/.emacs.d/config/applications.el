;;; applications.el --- Customisation for Emacs applications

;; Author: xoddf2 <woddfellow2@gmail.com>
;; Keywords: local
;; Version: 2.1
;; Time-stamp: <2015-02-14 00:50:03 PST xoddf2>

;;; Commentary:

;; This file contains custom settings for Emacs applications such as dired,
;; org, etc.

;;; Code:

;; dired
(if (eq system-type 'berkeley-unix)
    (progn
      (setq ls-lisp-use-insert-directory-program nil)
      (require 'ls-lisp)))

;; GPG
(require 'epa-file)

;; mu4e
(setq user-full-name "woddfellow2")
(setq user-mail-address "woddfellow2@gmail.com") ; Kludge for compose-mail

(if (string-equal system-name "rofldell.local")
    (progn
      (require 'mu4e)

      (setq read-mail-command 'mu4e
            mail-user-agent 'mu4e-user-agent)

      (setq mu4e-maildir "~/Mail/Gmail/"
            mu4e-sent-folder "/[Gmail].Sent Mail"
            mu4e-drafts-folder "/[Gmail].Drafts"
            mu4e-trash-folder "/[Gmail].Trash"
            mu4e-refile-folder "/[Gmail].All Mail")

      (setq mu4e-get-mail-command "offlineimap"
            mu4e-update-interval 900)

      (setq message-send-mail-function 'smtpmail-send-it
            send-mail-function (quote smtpmail-send-it)
            smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
            smtpmail-auth-credentials '(("smtp.gmail.com" 587 "woddfellow2@gmail.com"
                                         nil))
            smtpmail-default-smtp-server "smtp.gmail.com"
            smtpmail-smtp-server "smtp.gmail.com"
            smtpmail-smtp-service 587)

      (setq mu4e-sent-messages-behavior 'delete) ; For Gmail

      (setq mu4e-compose-signature (with-temp-buffer
                                     (insert-file-contents "~/.signature")
                                     (buffer-string)))
      (setq message-signature-file "~/.signature")))

;; Emacs-w3m
(if (string-equal system-name "rofldell.local")
    (progn
      (require 'w3m)
      (require 'w3m-search)

      (setq w3m-use-cookies t)
      (setq browse-url-browser-function 'w3m-browse-url)

      (w3m-lnum-mode 1) ; Conkeror-like hints

      (add-to-list 'w3m-search-engine-alist
                   '("duckduckgo" "https://duckduckgo.com/?q=%s"))
      (setq w3m-search-default-engine "duckduckgo")

      (setq w3m-fill-column 80)))

;; Calendar
(setq calendar-time-display-form
      '(24-hours ":" minutes
                 (if time-zone " (") time-zone (if time-zone ")"))
      calendar-date-style 'iso)

;; Org
(setq org-directory "~/doc/org"
      org-agenda-files "~/doc/org/agenda"
      org-link-abbrev-alist
      '(("ddg" . "https://duckduckgo.com/?q=")
        ("wp"  . "https://en.wikipedia.org/wiki/Special:Search/")
        ("yt"  . "https://www.youtube.com/watch?v=")))

;; Magit
(require 'magit)

;; twittering-mode
(if (string-equal system-name "rofldell.local")
    (progn
      (require 'twittering-mode)
      (setq twittering-use-master-password t)
      (setq twittering-status-format "%i %s: %T (%@ via %f)%r%R")
      (add-hook 'twittering-mode-hook
                (lambda ()
                  (twittering-icon-mode 1)))))

;; mediawiki.el
(if (string-equal system-name "rofldell.local")
    (progn
      (require 'mediawiki)

      (eval-after-load "mediawiki"
        '(add-to-list 'mediawiki-site-alist
                      '("Wikipedia" "http://en.wikipedia.org/w/"
                        "Xoddf2" "" "Main Page")))))

;;; applications.el ends here
