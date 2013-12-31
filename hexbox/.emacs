;;; .emacs --- xoddf2's Emacs init file

;; Author: xoddf2 <woddfellow2@gmail.com>
;; Keywords: local
;; Version: 1.12.1
;; Time-stamp: <2013-12-30 22:41:46 xoddf2>

;;; Commentary:

;; This Emacs init file is intended for use with GNU Emacs 24.3 under GNU/Linux
;; (Slackware 14.1).

;;; Code:

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

;; Update time stamp when saving
(add-hook 'before-save-hook 'time-stamp)

;; Backup and autosave directory
(setq temporary-file-directory "~/.emacs.d/tmp/")
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wlshot (&optional name-shot-p)
  "Takes a screen shot and then uploads it to wlair.us.to.
If `universal-argument' is called, it asks for a filename.
Requires TRAMP, scp, and ImageMagick."
  (interactive "P")
  (if (eq window-system 'x)
      (when (yes-or-no-p "Take a screen shot? ")
        (let (filename url)
          (if (equal name-shot-p nil)
              (setq filename (concat (format-time-string "%s") ".png"))
            (setq filename (read-from-minibuffer "Filename: ")))
          (shell-command (concat "import -window root ~/img/shots/" filename))
          (copy-file
           (concat "~/img/shots/" filename)
           "/scp:turtil.net:/srv/vhosts/wlair.us.to/public/remote/img/shots/")
          (setq url (concat "http://wlair.us.to/remote/img/shots/" filename))
          (kill-new url)
          (message url)))
    (message "This is not X.  You cannot take a screen shot here.")))

(defun erc-supybot-quotegrabs-grab ()
  "Inserts the grab command.  Intended for use in channels
that contain a Supybot with the QuoteGrabs module."
  (interactive)
  (cond
   ((string-equal (buffer-name) "#archlinux-offtopic")
       (insert "!grab "))
   ((string-equal (buffer-name) "#archlinux")
       (insert "!grab "))
   ((string-equal (buffer-name) "#wlair")
       (insert "!grab "))
   ((string-equal (buffer-name) "##slackware-offtopic")
       (insert "@grab "))
   ((string-equal (buffer-name) "#main")
       (insert "%grab "))))

;; Editing Modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
            (setq indent-tabs-mode t)))

;; BBCode
(require 'bbcode-mode)

;; FVWM
(if (string-equal system-name "hexbox.local")
    (progn
      (require 'fvwm-mode)
      (add-to-list 'auto-mode-alist '(".fvwm2rc" . fvwm-mode))))

;; Other Modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Gnus
(require 'epa-file)

(setq read-mail-command 'gnus
      mail-user-agent 'gnus-user-agent)

(setq user-mail-address "woddfellow2@gmail.com") ; Kludge for C-x m

;; ERC
(if (string-equal system-name "hexbox.local")
    (progn
      (require 'erc)
      (require 'znc)
      (require 'xoddf2-erc) ; This points to a private file.
      (setq erc-timestamp-format "[%H:%M:%S] "
            erc-fill-prefix "           "
            erc-insert-timestamp-function 'erc-insert-timestamp-left)
      (setq erc-auto-set-away t
            erc-autoaway-idle-seconds 300)
      (setq erc-keywords
            '("woddf2" "wodd" "woodf" "woof2" "jbqqs2" "2fddow" "xoddf2"))
      (setq erc-track-exclude-types
            '("JOIN" "KICK" "NICK" "PART" "QUIT" "MODE" "324" "329" "332" "333"
              "353" "477"))))

;; Calendar
(setq calendar-latitude 40.57667
      calendar-longitude -122.37028
      calendar-location-name "Redding, CA")

;; Magit
(require 'magit)

;; mediawiki.el
(require 'mediawiki)

(eval-after-load "mediawiki"
  '(add-to-list 'mediawiki-site-alist
                '("Wikipedia" "http://en.wikipedia.org/w/"
                  "Xoddf2" "" "Main Page")))

;; Emacs-w3m
(require 'w3m)
(setq w3m-use-cookies t)
(setq browse-url-browser-function 'w3m-browse-url)

;; Twittering Mode
(if (string-equal system-name "hexbox.local")
    (progn
      (setq twittering-use-master-password t)))

;; Keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; General
(global-set-key (kbd "C-x C-a") 'calendar)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-x 4 v") 'view-file-other-window)
(global-set-key (kbd "C-x 5 v") 'view-file-other-frame)
(global-set-key (kbd "C-x !") 'shell)
(global-set-key (kbd "C-x /") 'grep)
(global-set-key (kbd "C-x M") 'gnus)
(global-set-key (kbd "C-x c") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x j") 'eval-region)
(global-set-key (kbd "C-x p") 'wlshot)
(global-set-key (kbd "C-x V") 'view-file)
(global-set-key (kbd "C-x w") 'w3m)
(global-set-key (kbd "M-+") 'calc)

(define-key help-map "M" 'man)

;; Mode-specific
(if (string-equal system-name "hexbox.local")
    (progn
      (define-key erc-mode-map (kbd "C-c C-g") 'erc-supybot-quotegrabs-grab)))
