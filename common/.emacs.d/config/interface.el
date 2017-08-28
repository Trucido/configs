;;; interface.el --- Interface customisation

;; Author: xoddf2 <woddfellow2@gmail.com>
;; Keywords: local
;; Time-stamp: <2017-08-28 12:48:15 PDT xoddf2>

;;; Commentary:

;; This file makes some changes to Emacs's interface.

;;; Code:

;; Simplify the interface:
(menu-bar-mode -1) ; Toolbar and scroll bar are disabled in ~/.Xresources.

(setq use-dialog-box nil)

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-grab-mouse nil)

;; Do not display the splash buffer or the scratch buffer message
(setq inhibit-startup-screen t
      initial-scratch-message nil)

;; Make C-x C-c less dangerous
(setq confirm-kill-emacs 'yes-or-no-p)

;; Scrolling
(setq scroll-conservatively most-positive-fixnum
      scroll-preserve-screen-position t)

;; Misc interface settings
(setq visible-bell t)

;; Stretch cursor for tabs; indicate empty lines, buffer boundaries (GUI-only)
(setq x-stretch-cursor t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries t)

;; Helm
(use-package helm
  :ensure t

  :init
  (require 'helm-config)

  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x r b" . helm-filtered-bookmarks)

   :map help-map
   ("M" . helm-man-woman))

  :diminish helm-mode

  :config
  (helm-mode 1))

;; Highlight matching ()s (like Vim default)
(show-paren-mode 1)

;; Display both line and column numbers in mode line
(line-number-mode 1)
(column-number-mode 1)

;; Sort ibuffer list
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Emacs"
                (or
                 (mode . lisp-interaction-mode)
                 (mode . messages-buffer-mode)))
               ("Programming"
                (or
                 (mode . lisp-mode)
                 (mode . emacs-lisp-mode)
                 (mode . c-mode)
                 (mode . sh-mode)
                 (mode . cperl-mode)
                 (mode . python-mode)
                 (mode . haskell-mode)))
               ("Web Dev"
                (or
                 (mode . html-mode)
                 (mode . css-mode)
                 (mode . js-mode)))
               ("Doc"
                (or
                 (mode . text-mode)
                 (mode . bbcode-mode)
                 (mode . mediawiki-mode)
                 (mode . markdown-mode)
                 (mode . tex-mode)
                 (mode . plain-tex-mode)
                 (mode . texinfo-mode)
                 (mode . latex-mode)
                 (mode . doctex-mode)))
               ("Org"
                (or
                 (mode . org-mode)
                 (mode . org-agenda-mode)
                 (mode . diary-mode)))
               ("Conf"
                (or
                 (mode . conf-mode)
                 (mode . conf-unix-mode)
                 (mode . conf-space-mode)
                 (mode . conf-colon-mode)
                 (mode . conf-xdefaults-mode)
                 (mode . conf-windows-mode)
                 (mode . conf-javaprop-mode)
                 (mode . conf-ppd-mode)))
               ("Hexl"
                (mode . hexl-mode))
               ("Fundamental"
                (mode . fundamental-mode))
               ("DocView"
                (mode . doc-view-mode))
               ("Dired"
                (mode . dired-mode))
               ("Mail"
                (or
                 (mode . mu4e-main-mode)
                 (mode . mu4e-headers-mode)
                 (mode . mu4e-compose-mode)
                 (mode . mu4e-compose-org-mode)
                 (mode . mu4e-view-mode)
                 (mode . mu4e~update-mail-mode)
                 (mode . mu4e~main-toggle-mail-sending-mode)
                 (mode . mu4e-about-mode)))
               ("IRC"
                (mode . erc-mode))
               ("Twitter"
                (mode . twittering-mode))
               ("Documentation"
                (or
                 (mode . Info-mode)
                 (mode . Man-mode)
                 (mode . apropos-mode)
                 (mode . help-mode)
                 (mode . woman-mode)))
               ("REPL"
                (or
                 (mode . shell-mode)
                 (mode . eshell-mode)
                 (mode . inferior-emacs-lisp-mode)
                 (mode . inferior-python-mode)
                 (mode . term-mode)))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; Display time, load average, battery life, and biff in mode line
(if (string-equal (system-name) "nomad")
    (progn
      (setq display-time-24hr-format t
            display-time-interval 1
            display-time-format "%H:%M:%S")
      (setq display-time-mail-file t
            display-time-mail-directory "~/Mail/Gmail/INBOX/new/")
      (setq battery-mode-line-format " %b%p%%")

      (display-time-mode 1)
      (display-battery-mode 1)))

;; Theme
(if (string-equal (system-name) "nomad")
    (load-theme 'xoddf2 t))

;;; interface.el ends here
