;;; $Id: .emacs,v 1.0 2013/08/06 23:29:00 xoddf2 Exp $

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

;; Backup and autosave directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; C
(setq c-default-style "bsd"
      indent-tabs-mode t
      c-basic-offset 8)

;; Keybindings
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)

;; Gnus
(setq read-mail-command 'gnus
      mail-user-agent 'gnus-user-agent)

(setq user-mail-address "woddfellow2@gmail.com") ; Kludge for C-x m

;; Emacs Lisp packages
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; Calendar
(setq calendar-latitude 40.57667
      calendar-longitude -122.37028
      calendar-location-name "Redding, CA")
