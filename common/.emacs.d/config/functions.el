;;; functions.el --- Custom functions

;; Author: xoddf2 <woddfellow2@gmail.com>
;; Keywords: local
;; Time-stamp: <2018-03-16 22:07:11 PDT xoddf2>

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
  "Send input to the process, and then synchronise the shell history with
the comint input ring.  See `comint-send-input' and `comint-read-input-ring'
for more information."
  (interactive)
  (comint-send-input)
  (comint-read-input-ring))

(defun post-update ()
  "Insert a timestamp and the bold text 'Update:', useful for
editing forum posts.

TODO: Support both BBCode and HTML."
  (interactive)
  (insert (format-time-string "[%a %b %d %H:%M:%S %Z %Y] [b]Update:[/b] ")))

;;; functions.el ends here
