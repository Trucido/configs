;;; $Id: .emacs,v 1.2.1 2013/08/11 22:45:30 xoddf2 Exp $

;; Packages
(require 'epa-file)
(require 'package)

;; load-path
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Simplify the interface:
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(menu-bar-mode -1)

(setq use-dialog-box nil)

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-grab-mouse nil)

;; Disable splash buffer and change *scratch* buffer message
(setq inhibit-startup-screen t
      initial-scratch-message ";; *scratch*\n\n")

;; Make C-x C-c less dangerous
(setq confirm-kill-emacs 'yes-or-no-p)

;; Misc settings
(setq-default indicate-empty-lines t)
(setq scroll-conservatively most-positive-fixnum
      x-stretch-cursor t
      visible-bell t)
(iswitchb-mode 1)
(setq indent-tabs-mode t)

;; Backup and autosave directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; C
(setq c-default-style "bsd"
      c-basic-offset 8)

;; Shell
(setq sh-basic-offset 8)

;; Perl
(defalias 'perl-mode 'cperl-mode)
(setq cperl-indent-level 8
      cperl-continued-statement-offset 0)

;; WWW languages
(add-hook 'html-mode-hook
	  (lambda ()
	    (setq sgml-basic-offset 8)))
(setq css-indent-offset 8)

;; Keybindings
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)

;; Gnus
(setq read-mail-command 'gnus
      mail-user-agent 'gnus-user-agent)

(setq user-mail-address "woddfellow2@gmail.com") ; Kludge for C-x m

;; ERC
(setq erc-timestamp-format "[%H:%M:%S] "
      erc-fill-prefix "           "
      erc-insert-timestamp-function 'erc-insert-timestamp-left)
(setq erc-auto-set-away t
      erc-autoaway-idle-seconds 300)

;; Emacs Lisp packages
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; Calendar
(setq calendar-latitude 40.57667
      calendar-longitude -122.37028
      calendar-location-name "Redding, CA")
