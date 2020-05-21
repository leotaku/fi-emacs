;;; fi-helpers.el --- Additional user commands for fi-emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Leo Gaskin

;; Author: Leo Gaskin <leo.gaskin@brg-feldkirchen.at>
;; Created: 21 June 2019
;; Homepage: https://github.com/leotaku/fi-emacs
;; Keywords: fi-emacs, configuration, lisp
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'fi-subr)

(defun fi-make-gui-frame ()
  "Create a X window system frame.
This command also works when run from a daemonized Emacs instance."
  (interactive)
  (let ((frame
         (make-frame '((window-system . x)))))
    (with-selected-frame frame
      (run-hook-with-args 'after-make-frame-functions nil))))

(defun fi-undo-global (arg)
  "Undo some previous changes.
Repeat this command to undo more changes.
A numeric ARG serves as a repeat count.

Unlike the builtin `undo' this command ignores transient mark mode."
  (interactive "p")
  (transient-mark-mode -1)
  (undo arg)
  (transient-mark-mode 1))

(defun fi-undo-only-global (arg)
  "Undo some previous changes, ignoring previous undo calls.
Repeat this command to undo more changes.
A numeric ARG serves as a repeat count.

Unlike the builtin `undo-only' this command ignores transient mark mode."
  (interactive "p")
  (transient-mark-mode -1)
  (undo-only arg)
  (transient-mark-mode 1))

(defun fi-universal-quit ()
  "A version of the builtin `keyboard-quit' command that should work in any buffer."
  (interactive)
  (fi-simulate-key (kbd "C-g")))

(defun fi-do-nothing (&rest _)
  "A command that does absolutely nothing."
  (interactive))

(provide 'fi-helpers)

;;; fi-helpers.el ends here
