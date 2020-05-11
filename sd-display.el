;;; sd-dislay.el --- Tabulated startup display for sd.el -*- lexical-binding: t -*-

;; Author: Leo Gaskin <leo.gaskin@brg-feldkirchen.at>
;; Created: 19 July 2019
;; Homepage: https://github.com/leotaku/fi-emacs
;; Keywords: fi-emacs, configuration, lisp
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

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

(require 'sd)
(require 'tabulated-list)

;;; Code:

(define-derived-mode sd-display-tabulated-mode tabulated-list-mode
  "Sd Display Tabulated"
  "Mode for displaying sd startup results in a table."
  (setq tabulated-list-format [("Unit" 30 t) ("Dependencies" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header))

;;;###autoload
(defun sd-display-tabulated ()
  "Show the startup results in a sorted table."
  (interactive)
  (let ((buffer-name (format "*Sd Display %s*" "Tabulated")))
    (with-current-buffer (get-buffer-create buffer-name)
      (sd-display-tabulated-mode)
      (setq tabulated-list-entries 'sd-display--list-entries)
      (tabulated-list-print t)
      (switch-to-buffer (current-buffer)))))

(defun sd-display--list-entries ()
  (let (result)
    (dolist (unit sd-startup-list)
      (let* ((name (symbol-name (sd-unit-name unit)))
             (exclude (string-prefix-p "." name)))
        (if (not exclude)
            (push (sd-display--format-unit unit) result))))
    (nreverse result)))

(defun sd-display--format-unit (unit)
  (let* ((name (symbol-name (sd-unit-name unit)))
	     (name-color (sd-display--format-unit-name unit))
	     (deps (sd-unit-dependencies unit))
	     (deps-color (mapconcat (lambda (name)
				                  (sd-display--format-unit-name
				                   (assq name sd-startup-list)))
				                deps ", ")))
    (list name (vector name-color deps-color))))

(defun sd-display--format-unit-name (unit)
  (let* ((name (sd-unit-name unit))
	     (state (sd-unit-state unit))
	     (color (cond ((eq state 'avail)
		               '(:inherit warning))
		              ((eq state 'mention)
		               '(:inherit shadow))
		              ((eq state 'success)
		               '(:inherit success))
		              (t
		               '(:inherit error)))))
    (propertize (symbol-name name) 'font-lock-face color)))

(provide 'sd-display)

;;; sd-display.el ends here
