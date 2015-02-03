;;; interface.el --- Interface customisation

;; Author: xoddf2 <woddfellow2@gmail.com>
;; Keywords: local
;; Version: 2.0.5
;; Time-stamp: <2015-02-02 20:53:16 PST xoddf2>

;;; Commentary:

;; This file makes some changes to Emacs's interface.

;;; Code:

;; Simplify the interface:
(menu-bar-mode -1) ; Toolbar and scroll bar are disabled in ~/.Xresources.

(setq use-dialog-box nil)

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-grab-mouse nil)

;; Evil
(require 'evil)
(evil-mode 1)

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

;; Stretch cursor for tabs and indicate empty lines (requires GUI)
(setq x-stretch-cursor t)
(setq-default indicate-empty-lines t)

;; Ido mode
(ido-mode 1)
(setq ido-everywhere t)

;; Highlight matching ()s (like Vim default)
(show-paren-mode 1)

;; Display both line and column numbers in mode line
(line-number-mode 1)
(column-number-mode 1)

;; Display time and load average in mode line
(setq display-time-24hr-format t
      display-time-interval 1
      display-time-format "%H:%M:%S"
      display-time-mail-file t)
(display-time-mode 1)

;; Remove mode-line clutter
(require 'diminish)
(diminish 'magit-auto-revert-mode)
(diminish 'undo-tree-mode)
(diminish 'yas-minor-mode)

;; No 3D effect on mode line (in GUI)
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

;; Transparency in GUI (requires compositor)
(add-to-list 'default-frame-alist '(alpha 100 85))

;;; interface.el ends here
