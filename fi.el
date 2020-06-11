;;; fi.el --- Meta-package for fi-emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; This meta-package collectively loads the various facilities
;; provided by all packages in fi-emacs.  Note that you can also
;; install and load the individual packages separately.

(require 'bk)
(require 'sd)
(require 'sd-display)
(require 'fi-config)
(require 'fi-helpers)

;; Code:

(provide 'fi)

;;; fi.el ends here
