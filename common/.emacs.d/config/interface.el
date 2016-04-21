;;; interface.el --- Interface customisation

;; Author: xoddf2 <woddfellow2@gmail.com>
;; Keywords: local
;; Version: 2.0.11
;; Time-stamp: <2016-04-08 17:33:09 PDT xoddf2>

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

;; Ido mode
(ido-mode 1)
(setq ido-everywhere t)

;; Highlight matching ()s (like Vim default)
(show-paren-mode 1)

;; Display both line and column numbers in mode line
(line-number-mode 1)
(column-number-mode 1)

;; Display time, load average, and (on laptop) battery life in mode line
(setq display-time-24hr-format t
      display-time-interval 1
      display-time-format "%H:%M:%S"
      display-time-mail-file t
      battery-mode-line-format " %b%p%%")

(if (string-equal system-name "nomad.local")
    (setq display-time-mail-directory "~/Mail/Gmail/INBOX/new/"))

(display-time-mode 1)

(if (string-equal system-name "nomad.local")
    (display-battery-mode 1))

;; Remove mode-line clutter
(require 'diminish)
(diminish 'yas-minor-mode)

;; Theme
(if (string-equal system-name "nomad.local")
    (load-theme 'dakrone t))

;; No 3D effect on mode line (in GUI)
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

;;; interface.el ends here
