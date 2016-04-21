;;; general.el --- Miscellaneous settings

;; Author: xoddf2 <woddfellow2@gmail.com>
;; Keywords: local
;; Version: 2.0.6
;; Time-stamp: <2016-04-08 17:33:10 PDT xoddf2>

;;; Commentary:

;; This file contains Emacs Lisp code that does not belong in the other files.

;;; Code:

;; ELPA
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Time-stamp
(add-hook 'before-save-hook 'time-stamp)
(setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S %Z %u")

;; Backup and autosave directory
(setq temporary-file-directory "~/tmp/")
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; YASnippet
(require 'yasnippet)
(yas-global-mode 1)

(add-to-list 'yas/root-directory
             (concat (expand-file-name user-emacs-directory) "snippets"))

;; Do not use a GUI menu for yasnippet prompts in X
(setq yas-prompt-functions
      (cons 'yas-ido-prompt
            (remove 'yas-ido-prompt yas-prompt-functions)))

;;; general.el ends here
