;;; sd-dislay.el --- Tabulated startup display for sd.el -*- lexical-binding: t -*-

;; Copyright (C) 2019-2025 Leo Gaskin

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

;; This package provides a visual display of all defined sd-units.
;; It can be accessed by executing the `sd-display-tabulated` command.

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
  (let* ((result nil)
         (relations (sd--dependency-relations sd-startup-list))
         (sorted (sort relations (lambda (a b) (< (length a) (length b))))))
    (dolist (relation sorted result)
      (let* ((name (car relation))
             (unit (assq name sd-startup-list)))
        (push (sd-display--format-unit unit) result)))))

(defun sd-display--format-unit (unit)
  (let* ((name (sd-unit-name unit))
	     (name-color (sd-display--format-unit-name name))
	     (deps (sd-unit-dependencies unit))
	     (deps-color (mapconcat #'sd-display--format-unit-name deps ", ")))
    (list name (vector name-color deps-color))))

(defun sd-display--format-unit-name (name)
  (let* ((unit (assq name sd-startup-list))
	     (state (sd-unit-state unit))
	     (color (cond ((eq unit nil)
		               '(:inherit warning))
                      ((eq state 'available)
		               '(:inherit shadow))
		              ((eq state 'success)
		               '(:inherit success))
		              (t
		               '(:inherit error)))))
    (propertize (symbol-name name) 'font-lock-face color)))

(provide 'sd-display)

;;; sd-display.el ends here
