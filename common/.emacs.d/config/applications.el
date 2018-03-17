;;; applications.el --- Customisation for Emacs applications

;; Author: xoddf2 <woddfellow2@gmail.com>
;; Keywords: local
;; Time-stamp: <2018-03-16 22:07:03 PDT xoddf2>

;;; Commentary:

;; This file contains custom settings for Emacs applications such as dired,
;; org, etc.

;;; Code:

;; dired
(add-hook 'dired-load-hook
          '(lambda ()
             (require 'dired-x)

             (setq dired-listing-switches "-alh")

             (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))))

(add-hook 'dired-mode-hook
          '(lambda ()
             (dired-omit-mode 1)))

;; Shell
(require 'shell)

(setq comint-input-ring-size 10000)

(add-hook 'shell-mode-hook
          (lambda ()
            (setq comint-input-ring-file-name "~/.bash_history")
            (comint-read-input-ring t)))

;; GPG
(require 'epa-file)

;; mu4e
(use-package mu4e
  :config
  (setq user-full-name "woddfellow2")
  (setq user-mail-address "woddfellow2@gmail.com") ; Kludge for compose-mail

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
  (setq message-signature-file "~/.signature")

  (setq mu4e-bookmarks
        `( ,(make-mu4e-bookmark
             :name  "Unread messages"
             :query "flag:unread AND NOT flag:trashed AND NOT maildir:\"/[Gmail].All Mail\" AND NOT maildir:\"/[Gmail].Sent Mail\" AND NOT \"/[Gmail].Important\" AND NOT maildir:\"/[Gmail].Spam\" AND NOT \"maildir:/[Gmail].Trash\""
             :key ?u)
           ,(make-mu4e-bookmark
             :name "Today's messages"
             :query "date:today..now AND NOT \"/INBOX\" AND NOT maildir:\"/[Gmail].Sent Mail\" AND NOT maildir:\"/[Gmail].Spam\" AND NOT \"/[Gmail].Important\""
             :key ?t)
           ,(make-mu4e-bookmark
             :name "Last 7 days"
             :query "date:7d..now AND NOT \"/INBOX\" AND NOT maildir:\"/[Gmail].Sent Mail\" AND NOT maildir:\"/[Gmail].Spam\" AND NOT \"/[Gmail].Important\""
             :key ?w)
           ,(make-mu4e-bookmark
             :name "All Mail"
             :query "\"/[Gmail].All Mail\""
             :key ?a)
           ,(make-mu4e-bookmark
             :name "Sent messages"
             :query "maildir:\"/[Gmail].Sent Mail\""
             :key ?s)))
  :bind ("C-c m" . mu4e))

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
(use-package magit
  :bind (("C-x g" . magit-status)))

;; Mingus
(if (string-equal (system-name) "nomad")
    (use-package mingus
      :config
      (setq mingus-mpd-config-file "~/.config/mpd/mpd.conf"
            mingus-mpd-root "~/media/audio/music/"
            mingus-mpd-playlist-dir "~/.config/mpd/playlists"
            mingus-mode-line-string-max 30
            mingus-seek-amount 5)
      :bind ("C-c b" . mingus)))

;; twittering-mode
(if (string-equal (system-name) "nomad")
    (use-package twittering-mode
      :config
      (setq twittering-use-master-password t)
      (add-hook 'twittering-mode-hook
                (lambda ()
                  (twittering-icon-mode 1)))
      :bind ("C-c t" . twit)))

;;; applications.el ends here
