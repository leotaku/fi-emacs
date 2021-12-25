;;; fi-helpers.el --- Additional user commands for fi-emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021 Leo Gaskin

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

;; This package provides a number of useful interactive commands that
;; are missing from the Emacs standard library.  It only depends on
;; facilities that are already loaded per default.
;;
;; Please consult the individual elisp docstrings for documentation.

(require 'fi-config)

;;; Code:

(defun fi-make-gui-frame ()
  "Create a X window system frame.
This command also works when run from a daemonized Emacs instance."
  (interactive)
  (let ((frame
         (make-frame '((window-system . x)))))
    (with-selected-frame frame
      (run-hook-with-args 'after-make-frame-functions nil))))

(provide 'fi-helpers)

;;; fi-helpers.el ends here
