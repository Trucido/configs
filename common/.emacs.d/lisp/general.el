;;; general.el --- Miscellaneous settings

;; Author: xoddf2 <woddfellow2@gmail.com>
;; Keywords: local
;; Version: 2.0.1
;; Time-stamp: <2015-01-30 16:20:30 PST xoddf2>

;;; Commentary:

;; This file contains Emacs Lisp code that does not belong in the other files.

;;; Code:

;; ELPA
(if (eq emacs-major-version '24)
    (progn
      (require 'package)

      (add-to-list 'package-archives
		   '("melpa" . "http://melpa.org/packages/"))
      (package-initialize)))

;; Time-stamp
(add-hook 'before-save-hook 'time-stamp)
(setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S %Z %u")

;; Backup and autosave directory
(setq temporary-file-directory "~/.emacs.d/tmp/")
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Do not put M-x customize settings in the init file
(setq custom-file "~/.emacs.d/lisp/custom.el")
(load custom-file)

;;; general.el ends here
