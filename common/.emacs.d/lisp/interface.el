;;; interface.el --- Interface customisation

;; Author: xoddf2 <woddfellow2@gmail.com>
;; Keywords: local
;; Version: 2.0.2
;; Time-stamp: <2015-01-27 15:24:37 PST xoddf2>

;;; Commentary:

;; This file makes some changes to Emacs's interface.

;;; Code:

;; Simplify the interface:
(menu-bar-mode -1) ; Toolbar and scroll bar are disabled in ~/.Xresources.

(setq use-dialog-box nil)

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-grab-mouse nil)

;; Disable splash buffer
(setq inhibit-startup-screen t)

;; Make C-x C-c less dangerous
(setq confirm-kill-emacs 'yes-or-no-p)

;; Misc interface settings
(setq scroll-conservatively most-positive-fixnum
      visible-bell t)

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

;; In the GUI, resize more like a terminal emulator
(setq frame-resize-pixelwise nil)

;; Eye candy (requires GUI)
(add-to-list 'default-frame-alist '(alpha 100 85))

;;; interface.el ends here
