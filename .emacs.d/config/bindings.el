;;; bindings.el --- Keybindings and mouse bindings

;; Author: xoddf2 <woddfellow2@gmail.com>
;; Keywords: local
;; Time-stamp: <2018-03-17 21:01:47 PDT xoddf2>

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
(global-set-key (kbd "C-c d") 'calendar)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Unix
(global-set-key (kbd "C-c z") 'shell)
(global-set-key (kbd "C-c g") 'grep)
(global-set-key (kbd "C-c x") 'compile)

;; Shell
(define-key shell-mode-map (kbd "RET") 'comint-send-input-and-update-input-ring)

;;; bindings.el ends here
