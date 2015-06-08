;;; bindings.el --- Keybindings and mouse bindings

;; Author: xoddf2 <woddfellow2@gmail.com>
;; Keywords: local
;; Version: 2.0.6
;; Time-stamp: <2015-06-07 17:56:07 PDT xoddf2>

;;; Commentary:

;; This file sets keybindings and mouse bindings (if any).

;;; Code:

;; General
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)

;; View file
(global-set-key (kbd "C-x 4 v") 'view-file-other-window)
(global-set-key (kbd "C-x 5 v") 'view-file-other-frame)
(global-set-key (kbd "C-c v") 'view-file)

;; Scratch buffer
(global-set-key (kbd "C-c s") 'switch-to-scratch-buffer)
(global-set-key (kbd "C-c 4 s") 'switch-to-scratch-buffer-other-window)
(global-set-key (kbd "C-c 5 s") 'switch-to-scratch-buffer-other-frame)

;; Applications
(global-set-key (kbd "C-c m") 'mu4e)
(global-set-key (kbd "C-c r") 'elfeed)
(global-set-key (kbd "C-c w") 'w3m)
(global-set-key (kbd "C-c d") 'calendar)
(global-set-key (kbd "C-c t") 'twit)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Unix
(global-set-key (kbd "C-c z") 'shell)
(global-set-key (kbd "C-c g") 'grep)
(global-set-key (kbd "C-c x") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)

;; Help
(define-key help-map "M" 'man)

;; BBCode
(if (string-equal system-name "rofldell.local")
    (define-key bbcode-mode-map (kbd "C-c C-u") 'post-update))

;;; bindings.el ends here
