;;; xoddf2-theme.el --- xoddf2's custom theme

;; Author: xoddf2 <woddfellow2@gmail.com>
;; Keywords: local
;; Version: 1.0
;; Time-stamp: <2016-10-26 16:18:47 PDT xoddf2>

;;; Commentary:

;; To be written...

;;; Code:

(deftheme xoddf2
  "xoddf2's personal theme")

(custom-theme-set-faces
 'xoddf2

 ;; General
 '(default ((t (:foreground "#E5E5E5" :background "#242424"))))
 '(foreground-color ((t (:foreground "#E5E5E5"))))
 ;; '(mode-line ((t (:foreground "#2A2A2A" :background "#8BDE58" :box nil))))
 '(mode-line ((t (:foreground "#2A2A2A" :background "#D9D9D9" :box nil))))
 '(mode-line-inactive ((t (:foreground "#E5E5E5" :background "#515151" :box nil))))
 '(region ((t (:foreground "#E5E5E5" :background "#23415D"))))
 '(minibuffer-prompt ((t (:foreground "#7FE0DC"))))
 '(link ((t (:foreground "#7FE0DC" :underline t))))
 '(link-visited ((t (:foreground "#D37CDC" :underline t))))
 '(isearch ((t (:foreground "#87222A" :background "#D37CDC"))))
 '(isearch-fail ((t (:foreground "#E5E5E5" :background "#87222A"))))
 '(query-replace ((t (:foreground "#87222A" :background "#D37CDC"))))
 '(show-paren-match ((t (:foreground "#E5E5E5" :background "#23415D"))))
 '(show-paren-mismatch ((t (:foreground "#E5E5E5" :background "#87222A"))))
 '(error ((t (:foreground "#D37CDC" :bold t))))
 '(success ((t (:foreground "#8BDE58" :bold t))))
 '(match ((t (:foreground "#E5E5E5" :background "#23415D"))))

 ;; Syntax highlighting
 '(font-lock-comment-face ((t (:foreground "#7DACDE"))))
 '(font-lock-string-face ((t (:foreground "#8BDE58"))))
 '(font-lock-variable-name-face ((t (:foreground "#DBDF39"))))
 '(font-lock-keyword-face ((t (:foreground "#7FE0DC"))))
 '(font-lock-type-face ((t (:foreground "#69943B"))))
 '(font-lock-warning-face ((t (:foreground "#DD424C" :bold t))))

 ;; CPerl mode
 '(cperl-array-face ((t (:foreground "#DBDF39" :background "#23415D"))))
 '(cperl-hash-face ((t (:foreground "#DD424C" :background "#23415D" :italic t))))
 '(cperl-nonoverridable-face ((t (:foreground "#D37CDC"))))

 ;; ansi-term
 '(term-color-black ((t (:foreground "#2A2A2A" :background "#2A2A2A"))))
 '(term-color-blue ((t (:foreground "#23415D" :background "#23415D"))))
 '(term-color-cyan ((t (:foreground "#407D78" :background "#407D78"))))
 '(term-color-green ((t (:foreground "#69943B" :background "#69943B"))))
 '(term-color-magenta ((t (:foreground "#6F3971" :background "#6F3971"))))
 '(term-color-red ((t (:foreground "#87222A" :background "#87222A"))))
 '(term-color-white ((t (:foreground "#929292" :background "#929292"))))
 '(term-color-yellow ((t (:foreground "#B8AB1D" :background "#B8AB1D"))))

 ;; Compilation
 '(compliation-column-number ((t (:foreground "#69943B"))))
 '(compilation-line-number ((t (:foreground "#407D78"))))
 '(compilation-mode-line-exit ((t (:foreground "#69943B" :bold t))))
 '(compilation-mode-line-fail ((t (:foreground "#DD424C" :bold t))))
 '(compilation-warning ((t (:foreground "#B8AB1D" :bold t))))

 ;; Org
 '(org-date ((t (:foreground "#7FE0DC" :underline t))))
 '(org-done ((t (:foreground "#8BDE58" :bold t))))
 '(org-todo ((t (:foreground "#DD424C" :bold t))))
 '(org-level-1 ((t (:foreground "#B8AB1D"))))
 '(org-level-2 ((t (:foreground "#DBDF39"))))
 '(org-level-3 ((t (:foreground "#8BDE58"))))
 '(org-level-4 ((t (:foreground "#7DACDE"))))
 '(org-level-5 ((t (:foreground "#407D78"))))
 '(org-level-6 ((t (:foreground "#7FE0DC"))))
 '(org-level-7 ((t (:foreground "#6F3971"))))
 '(org-level-8 ((t (:foreground "#D37CDC"))))
 '(org-footnote ((t (:inherit org-date))))
 '(org-table ((t (:foreground "#7DACDE"))))
 '(org-mode-line-clock-overrun ((t (:background "#DD424C"))))

 ;; Info
 '(info-menu-star ((t (:foreground "#DD424C"))))

 ;; Ido
 '(ido-indicator ((t (:foreground "#DBDF39" :background "#87222A"))))
 '(ido-only-match ((t (:foreground "#69943B"))))
 '(ido-subdir ((t (:foreground "#DD424C"))))

 ;; ediff
 '(ediff-current-diff-A ((t (:foreground "#E5E5E5" :background "#87222A"))))
 '(ediff-current-diff-Ancestor ((t (:foreground "#242424" :background "#D37CDC"))))
 '(ediff-current-diff-B ((t (:foreground "#E5E5E5" :background "#69943B"))))
 '(ediff-current-diff-C ((t (:foreground "#E5E5E5" :background "#B8AB1D"))))
 '(ediff-fine-diff-A ((t (:foreground "#E5E5E5" :background "#DD424C"))))
 '(ediff-fine-diff-Ancestor ((t (:foreground "#242424" :background "#8BDE58"))))
 '(ediff-fine-diff-B ((t (:foreground "#E5E5E5" :background "#69943B"))))
 '(ediff-fine-diff-C ((t (:foreground "#E5E5E5" :background "#B8AB1D"))))

 ;; eshell
 '(eshell-ls-archive ((t (:foreground "#D37CDC" :bold t))))
 '(eshell-ls-backup ((t (:foreground "#87222A"))))
 '(eshell-ls-clutter ((t (:foreground "#DD424C" :bold t))))
 '(eshell-ls-directory ((t (:foreground "#7FE0DC" :bold t))))
 '(eshell-ls-executable ((t (:foreground "#8BDE58" :bold t))))
 '(eshell-ls-missing ((t (:foreground "#87222A" :bold t))))
 '(eshell-ls-product ((t (:foreground "#87222A"))))
 '(eshell-ls-readonly ((t (:foreground "#6F3971"))))
 '(eshell-ls-special ((t (:foreground "#D37CDC" :bold t))))
 '(eshell-ls-symlink ((t (:foreground "#7FE0DC" :bold t))))
 '(eshell-ls-unreadable ((t (:foreground "#929292"))))
 '(eshell-prompt ((t (:foreground "#8BDE58" :bold t))))

 ;; Whitespace
 '(trailing-whitespace ((t (:foreground "#E5E5E5" :background "#DD424C"))))
 '(whitespace-empty ((t (:foreground "#87222A" :background "#DBDF39"))))
 '(whitespace-indentation ((t (:foreground "#87222A" :background "#DBDF39"))))
 '(whitespace-space-after-tab ((t (:foreground "#87222A" :background "#DBDF39"))))
 '(whitespace-space-before-tab ((t (:foreground "#87222A" :background "#B8AB1D"))))
 '(whitespace-trailing ((t (:foreground "#DBDF39" :background "#DD424C" :bold t)))))
 
(provide-theme 'xoddf2)

;;; xoddf2-theme.el ends here
