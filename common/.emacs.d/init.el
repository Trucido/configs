;;; init.el --- Loads Emacs config

;; Author: xoddf2 <woddfellow2@gmail.com>
;; Keywords: local
;; Version: 2.0.6
;; Time-stamp: <2017-06-19 03:29:40 PDT xoddf2>

;;; Commentary:

;; This file loads the rest of the Emacs config.

;;; Code:

;; load-path


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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

;;; init.el ends here
