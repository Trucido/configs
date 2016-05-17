;;; editing.el --- Customisation for major modes

;; Author: xoddf2 <woddfellow2@gmail.com>
;; Keywords: local
;; Version: 2.0.3
;; Time-stamp: <2016-05-17 01:44:26 PDT xoddf2>

;;; Commentary:

;; This file contains custom settings for editing files.

;;; Code:

;; General
(setq require-final-newline t) ; Add missing newline when saving

;; Lisp
(add-hook 'lisp-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)))

;; C
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-default-style "bsd")
            (setq c-basic-offset 8)
            (setq indent-tabs-mode t)))

;; Shell
(add-hook 'sh-mode-hook
          (lambda ()
            (setq sh-basic-offset 8)
            (setq sh-indentation 8)
            (setq indent-tabs-mode t)))

;; Perl
(defalias 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq cperl-close-paren-offset -4)
            (setq cperl-continued-statement-offset 4)
            (setq cperl-indent-level 4)
            (setq cperl-indent-parens-as-block t)
            (setq cperl-tab-always-indent t)))

;; Python
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq python-indent-offset 4)
            (set-fill-column 79)
            (auto-fill-mode 1)))

;; WWW languages
(add-hook 'html-mode-hook
          (lambda ()
            (setq sgml-basic-offset 8)
            (setq indent-tabs-mode t)))
(add-hook 'css-mode-hook
          (lambda ()
            (setq css-indent-offset 8)
            (setq indent-tabs-mode t)
            (rainbow-mode 1)))

;; Text
(add-hook 'text-mode-hook
          (lambda ()
            (flyspell-mode 1)))

;; BBCode
(if (string-equal system-name "nomad.local")
    (progn
      (require 'bbcode-mode)))

;;; editing.el ends here
