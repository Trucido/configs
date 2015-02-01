;;; init.el --- Loads Emacs config

;; Author: xoddf2 <woddfellow2@gmail.com>
;; Keywords: local
;; Version: 2.0.3
;; Time-stamp: <2015-01-31 18:09:22 PST xoddf2>

;;; Commentary:

;; This file loads the rest of the Emacs config.

;;; Code:

;; load-path
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
(add-to-list 'load-path (concat (expand-file-name user-emacs-directory) "lisp"))

(unless (eq emacs-major-version '24)
  (let ((default-directory
          (concat (expand-file-name user-emacs-directory "packages"))))
    (normal-top-level-add-subdirs-to-load-path)))

(load-library "general")
(load-library "interface")
(load-library "functions")
(load-library "editing")
(load-library "applications")
(load-library "bindings")
(load-library "private")

;;; init.el ends here
