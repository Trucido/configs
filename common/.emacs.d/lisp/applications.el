;;; applications.el --- Customisation for Emacs applications

;; Author: xoddf2 <woddfellow2@gmail.com>
;; Keywords: local
;; Version: 2.0.3
;; Time-stamp: <2015-01-30 16:53:42 PST xoddf2>

;;; Commentary:

;; This file contains custom settings for Emacs applications such as dired,
;; Gnus, etc.

;;; Code:

;; dired
(if (eq system-type 'berkeley-unix)
    (progn
      (setq ls-lisp-use-insert-directory-program nil)
      (require 'ls-lisp)))

;; Gnus
(require 'epa-file)

(setq read-mail-command 'gnus
      mail-user-agent 'gnus-user-agent)

(setq user-mail-address "woddfellow2@gmail.com") ; Kludge for compose-mail

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
(if (string-equal system-name "rofldell.local")
    (require 'magit))

;; mediawiki.el
(if (string-equal system-name "rofldell.local")
    (progn
      (require 'mediawiki)

      (eval-after-load "mediawiki"
        '(add-to-list 'mediawiki-site-alist
                      '("Wikipedia" "http://en.wikipedia.org/w/"
                        "Xoddf2" "" "Main Page")))))

;;; applications.el ends here
