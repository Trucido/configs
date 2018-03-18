;;; functions.el --- Custom functions

;; Author: xoddf2 <woddfellow2@gmail.com>
;; Keywords: local
;; Time-stamp: <2018-03-17 20:43:08 PDT xoddf2>

;;; Commentary:

;; This file contains custom functions.

;;; Code:

(defun switch-to-scratch-buffer ()
  "Switches to the *scratch* buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (unless (equal mode-name "Lisp Interaction")
    (lisp-interaction-mode)))

(defun switch-to-scratch-buffer-other-window ()
  "Switches to the *scratch* buffer.  Like `switch-to-scratch-buffer' but
switches to the *scratch* buffer in another window."
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
  (unless (equal mode-name "Lisp Interaction")
    (lisp-interaction-mode)))

(defun switch-to-scratch-buffer-other-frame ()
  "Switches to the *scratch* buffer.  Like `switch-to-scratch-buffer' but
switches to the *scratch* buffer in another frame."
  (interactive)
  (switch-to-buffer-other-frame (get-buffer-create "*scratch*"))
  (unless (equal mode-name "Lisp Interaction")
    (lisp-interaction-mode)))

(defun comint-send-input-and-update-input-ring ()
  "Synchronise the shell history with the comint input ring before sending
input to the process.  See `comint-read-input-ring' and `comint-send-input'
for more information."
  (interactive)
  (comint-read-input-ring)
  (comint-send-input))

(defun post-update ()
  "Insert a timestamp and the bold text 'Update:', useful for
editing forum posts.

TODO: Support both BBCode and HTML."
  (interactive)
  (insert (format-time-string "[%a %b %d %H:%M:%S %Z %Y] [b]Update:[/b] ")))

;;; functions.el ends here
