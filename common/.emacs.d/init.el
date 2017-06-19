;;; init.el --- Loads Emacs config

;; Author: xoddf2 <woddfellow2@gmail.com>
;; Keywords: local
;; Time-stamp: <2017-06-19 04:47:39 PDT xoddf2>

;;; Commentary:

;; This file loads the rest of the Emacs config.

;;; Code:

;; To prevent package.el and M-x customize from tampering with this file
(setq package--init-file-ensured t)
(setq custom-file "~/.emacs.d/config/custom.el")
(load custom-file)

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

;;; init.el ends here
