;;; init.el --- Loads Emacs config

;; Author: xoddf2 <woddfellow2@gmail.com>
;; Keywords: local
;; Version: 2.0.1
;; Time-stamp: <2015-01-29 06:24:15 PST xoddf2>

;;; Commentary:

;; This file loads the rest of the Emacs config.

;;; Code:

;; load-path
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
(add-to-list 'load-path (concat (expand-file-name user-emacs-directory) "lisp"))
(if (string-equal system-name "elisabeth")
    (add-to-list 'load-path
                 (concat (expand-file-name user-emacs-directory) "evil")))

(load-library "general")
(load-library "interface")
(load-library "functions")
(load-library "editing")
(load-library "applications")
(load-library "bindings")

;;; init.el ends here
