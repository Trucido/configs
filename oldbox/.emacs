;;; $Id: .emacs,v 1.8.5 2013/09/30 16:35:31 xoddf2 Exp $

;; This Emacs init file is intended for use with GNU Emacs 24.3 under GNU/Linux
;; (Slackware 14.0).  It is not guaranteed to work elsewhere without
;; modification.
;;
;; Dependencies:
;; - From ELPA: Emacs-w3m, bbcode-mode, twittering-mode, mediawiki-mode,
;;              rainbow-mode
;; - Other: fvwm-mode, magit
;;
;; TODO:
;; - Make Emacs-w3m the default browser.
;; - Put backup and auto-save files somewhere in ~/.emacs.d.
;; - Reduce the redundancy in lookup-*.

;; General ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load-path
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; ELPA
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; Simplify the interface:
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(menu-bar-mode -1)

(setq use-dialog-box nil)

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-grab-mouse nil)

;; Disable splash buffer and change *scratch* buffer message
(setq inhibit-startup-screen t
      initial-scratch-message ";; *scratch*\n\n")

;; Make C-x C-c less dangerous
(setq confirm-kill-emacs 'yes-or-no-p)

;; Misc settings
(setq-default indicate-empty-lines t)
(setq scroll-conservatively most-positive-fixnum
      x-stretch-cursor t
      visible-bell t)
(iswitchb-mode 1)
(setq indent-tabs-mode t)

;; Backup and autosave directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-timestamp (id version)
  "Insert a timestamp into the buffer, like this:

// $Id: foo.c,v 1.0 2007/07/07 13:37:42 someperson Exp $"
  (interactive
   (list
    (read-from-minibuffer "Id (default Id): " "Id")
    (read-from-minibuffer "Version: ")))
  (insert (concat "$" id ": " (buffer-name) ",v " version " "
                  (format-time-string "%Y/%m/%d %H:%M:%S") " " user-login-name
                  " Exp $")))

(defun post-update ()
  "This inserts a timestamp and the bold text 'Update:', useful for
editing forum posts."
  (interactive)
  (insert (format-time-string "[%a %b %d %H:%M:%S %Z %Y] [b]Update:[/b] ")))

(defun full-screen ()
  "Borrowed from URL `http://mikerowecode.com/2009/05/emacs-full-screen.html'.
Toggles the Emacs frame full-screen, if running in X.  Requires a
standards-compliant window manager."
  (interactive)
  (if (eq window-system 'x)
      (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                               nil
                                             'fullboth))))

(defun lookup-duckduckgo ()
  "From URL `http://ergoemacs.org/emacs/emacs_lookup_ref.html'.
Searches for the region (or word at point) at DuckDuckGo.
Requires Emacs-w3m."
  (interactive)
  (let (word url)
    (setq word
          (if (region-active-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (thing-at-point 'symbol)))
    (setq word (replace-regexp-in-string " " "+" word))
    (setq url
          (concat "https://duckduckgo.com/lite/?q=" word))
    (w3m-browse-url url)))

(defun lookup-wikipedia ()
  "From URL `http://ergoemacs.org/emacs/emacs_lookup_ref.html'.
Searches for the region (or word at point) at Wikipedia.
Requires Emacs-w3m."
  (interactive)
  (let (word url)
    (setq word
          (if (region-active-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (thing-at-point 'symbol)))
    (setq word (replace-regexp-in-string " " "+" word))
    (setq url
          (concat "https://en.wikipedia.org/wiki/Special:Search?search=" word))
    (w3m-browse-url url)))

(defun lookup-wiktionary ()
  "From URL `http://ergoemacs.org/emacs/emacs_lookup_ref.html'.
Searches for the region (or word at point) at Wiktionary.
Requires Emacs-w3m."
  (interactive)
  (let (word url)
    (setq word
          (if (region-active-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (thing-at-point 'symbol)))
    (setq word (replace-regexp-in-string " " "+" word))
    (setq url
          (concat "https://en.wiktionary.org/wiki/Special:Search?search=" word))
    (w3m-browse-url url)))

(defun lookup-emacswiki ()
  "From URL `http://ergoemacs.org/emacs/emacs_lookup_ref.html'.
Searches for the region (or word at point) at EmacsWiki.
Requires Emacs-w3m."
  (interactive)
  (let (word url)
    (setq word
          (if (region-active-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (thing-at-point 'symbol)))
    (setq word (replace-regexp-in-string " " "+" word))
    (setq url
          (concat
           "https://duckduckgo.com/lite/?q=" word "+site%3Aemacswiki.org"))
    (w3m-browse-url url)))

;; Editing Modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lisp
(add-hook 'lisp-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)))

;; C
(setq c-default-style "bsd"
      c-basic-offset 8)

;; Shell
(setq sh-basic-offset 8)

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
            (setq sgml-basic-offset 8)))
(setq css-indent-offset 8)

;; BBCode
(require 'bbcode-mode)

;; FVWM
(require 'fvwm-mode)
(add-to-list 'auto-mode-alist '(".fvwm2rc" . fvwm-mode))

;; Other Modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Gnus
(require 'epa-file)

(setq read-mail-command 'gnus
      mail-user-agent 'gnus-user-agent)

(setq user-mail-address "woddfellow2@gmail.com") ; Kludge for C-x m

;; ERC
(setq erc-timestamp-format "[%H:%M:%S] "
      erc-fill-prefix "           "
      erc-insert-timestamp-function 'erc-insert-timestamp-left)
(setq erc-auto-set-away t
      erc-autoaway-idle-seconds 300)

;; Emacs-w3m
(require 'w3m)
(setq w3m-use-cookies t)

;; Twittering Mode
(setq twittering-use-master-password t)

;; mediawiki.el
(require 'mediawiki)

(eval-after-load "mediawiki"
  '(add-to-list 'mediawiki-site-alist
                '("Wikipedia" "http://en.wikipedia.org/w/"
                  "Xoddf2" "" "Main Page")))

;; Calendar
(setq calendar-latitude 40.57667
      calendar-longitude -122.37028
      calendar-location-name "Redding, CA")

;; Magit
(require 'magit)

;; Keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; General
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-x 4 v") 'view-file-other-window)
(global-set-key (kbd "C-x 5 v") 'view-file-other-frame)
(global-set-key (kbd "C-x t") 'insert-timestamp)
(global-set-key (kbd "<f11>") 'full-screen)

;; Mode-specific
(define-key bbcode-mode-map (kbd "C-c C-u") 'post-update)

;; Frequently-used commands
(defvar apps-map (make-sparse-keymap)
  "Keymap for frequently-used commands.")
(defalias 'apps-prefix apps-map)
(define-key ctl-x-map "x" 'apps-prefix)

(define-key apps-map "!" 'shell)
(define-key apps-map "#" 'calc)
(define-key apps-map "$" 'eshell)
(define-key apps-map "/" 'grep)
(define-key apps-map "a" 'calendar)
(define-key apps-map "c" 'compile)
(define-key apps-map "d" 'image-dired)
(define-key apps-map "e" 'eval-region)
(define-key apps-map "f" 'auto-fill-mode)
(define-key apps-map "h" 'man)
(define-key apps-map "i" 'whitespace-mode)
(define-key apps-map "l" 'lunar-phases)
(define-key apps-map "m" 'gnus)
(define-key apps-map "r" 'rainbow-mode)
(define-key apps-map "s" 'sunrise-sunset)
(define-key apps-map "t" 'twit)
(define-key apps-map "v" 'view-file)
(define-key apps-map "w" 'w3m)

;; Look up region or word at point
(defvar lookup-map (make-sparse-keymap)
  "Keymap for subcommands of C-x w.")
(defalias 'lookup-prefix lookup-map)
(define-key ctl-x-map "w" 'lookup-prefix)

(define-key lookup-map "g" 'lookup-duckduckgo)
(define-key lookup-map "w" 'lookup-wikipedia)
(define-key lookup-map "d" 'lookup-wiktionary)
(define-key lookup-map "e" 'lookup-emacswiki)
