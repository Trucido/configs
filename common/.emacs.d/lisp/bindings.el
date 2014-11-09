;;; bindings.el --- Keybindings and mouse bindings

;; Author: xoddf2 <woddfellow2@gmail.com>
;; Keywords: local
;; Version: 2.0
;; Time-stamp: <2014-11-09 00:07:03 PST xoddf2>

;;; Commentary:

;; This file sets keybindings and mouse bindings (if any).

;;; Code:

;; General
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-x j") 'eval-region)

;; View file
(global-set-key (kbd "C-x 4 v") 'view-file-other-window)
(global-set-key (kbd "C-x 5 v") 'view-file-other-frame)
(global-set-key (kbd "C-x V") 'view-file)

;; Applications
(global-set-key (kbd "C-x M") 'gnus)
(global-set-key (kbd "C-x w") 'w3m)
(global-set-key (kbd "M-+") 'calc)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Unix
(global-set-key (kbd "C-x !") 'shell)
(global-set-key (kbd "C-x /") 'grep)
(global-set-key (kbd "C-x c") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)

;; Help
(define-key help-map "M" 'man)

;;; bindings.el ends here
