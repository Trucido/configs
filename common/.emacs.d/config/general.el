;;; general.el --- Miscellaneous settings

;; Author: xoddf2 <woddfellow2@gmail.com>
;; Keywords: local
;; Time-stamp: <2017-06-19 04:48:38 PDT xoddf2>

;;; Commentary:

;; This file contains Emacs Lisp code that does not belong in the other files.

;;; Code:

;; ELPA
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'diminish)
(require 'bind-key)

;; Time-stamp
(add-hook 'before-save-hook 'time-stamp)
(setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S %Z %u")

;; Backup and autosave directory
(setq temporary-file-directory "~/tmp/")
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; YASnippet
(use-package yasnippet
  :config
  (yas-global-mode 1)

  (add-to-list 'yas/root-directory
               (concat (expand-file-name user-emacs-directory) "snippets"))

  ;; Do not use a GUI menu for yasnippet prompts in X
  (setq yas-prompt-functions
        (cons 'yas-ido-prompt
              (remove 'yas-ido-prompt yas-prompt-functions)))

  :diminish yas-minor-mode)

;;; general.el ends here
