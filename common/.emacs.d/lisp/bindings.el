;;; bindings.el --- Keybindings and mouse bindings

;; Author: xoddf2 <woddfellow2@gmail.com>
;; Keywords: local
;; Version: 2.0.1
;; Time-stamp: <2014-12-26 21:39:09 PST xoddf2>

;;; Commentary:

;; This file sets keybindings and mouse bindings (if any).

;;; Code:

;; General
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)

;; View file
(global-set-key (kbd "C-x 4 v") 'view-file-other-window)
(global-set-key (kbd "C-x 5 v") 'view-file-other-frame)
(global-set-key (kbd "C-x V") 'view-file)

;; Applications
(global-set-key (kbd "C-x m") 'gnus)
(global-set-key (kbd "C-x w") 'w3m)
(global-set-key (kbd "M-+") 'calc)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Unix
(global-set-key (kbd "C-x /") 'grep)
(global-set-key (kbd "C-x c") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)

;; Help
(define-key help-map "M" 'man)

;; BBCode
(if (string-equal system-name "rofldell.local")
    (define-key bbcode-mode-map (kbd "C-c C-u") 'post-update))

;;; bindings.el ends here
