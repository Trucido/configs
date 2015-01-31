;;; init.el --- Loads Emacs config

;; Author: xoddf2 <woddfellow2@gmail.com>
;; Keywords: local
;; Version: 2.0.2
;; Time-stamp: <2015-01-30 16:28:51 PST xoddf2>

;;; Commentary:

;; This file loads the rest of the Emacs config.

;;; Code:

;; load-path
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
(add-to-list 'load-path (concat (expand-file-name user-emacs-directory) "lisp"))
(unless (eq emacs-major-version '24)
  (add-to-list 'load-path
               (concat (expand-file-name user-emacs-directory) "evil")))

(load-library "general")
(load-library "interface")
(load-library "functions")
(load-library "editing")
(load-library "applications")
(load-library "bindings")
(load-library "private")

;;; init.el ends here
