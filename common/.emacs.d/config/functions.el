;;; functions.el --- Custom functions

;; Author: xoddf2 <woddfellow2@gmail.com>
;; Keywords: local
;; Version: 2.0.1
;; Time-stamp: <2014-12-25 01:51:05 PST xoddf2>

;;; Commentary:

;; This file contains custom functions.

;;; Code:

(defun post-update ()
  "Insert a timestamp and the bold text 'Update:', useful for
editing forum posts.

TODO: Support both BBCode and HTML."
  (interactive)
  (insert (format-time-string "[%a %b %d %H:%M:%S %Z %Y] [b]Update:[/b] ")))

;;; functions.el ends here
