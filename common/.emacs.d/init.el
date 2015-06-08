;;; init.el --- Loads Emacs config

;; Author: xoddf2 <woddfellow2@gmail.com>
;; Keywords: local
;; Version: 2.0.5
;; Time-stamp: <2015-06-07 17:55:05 PDT xoddf2>

;;; Commentary:

;; This file loads the rest of the Emacs config.

;;; Code:

;; load-path
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
(add-to-list 'load-path (concat (expand-file-name user-emacs-directory) "config"))
(add-to-list 'load-path (concat (expand-file-name user-emacs-directory) "lisp"))

(load-library "general")
(load-library "functions")
(load-library "editing")
(load-library "applications")
(load-library "interface")
(load-library "bindings")
(load-library "private")

;; Do not put M-x customize settings in the init file
(setq custom-file "~/.emacs.d/config/custom.el")
(load custom-file)

;; Open a shell when Emacs starts
(shell)

;;; init.el ends here
