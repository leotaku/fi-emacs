;; fi.el --- Fast iteration for your emacs configuration. -*- lexical-binding: t -*-

(defun get-path-in-buffer (path)
  (if (null path)
      (list (point-min) (point-max))
    (save-excursion
      (let ((start 0)
	    (stop 0))
	(setf (point) (point-min))
	(--each (nreverse path)
	  (let ((start-hops (if (consp it)
				(car it)
			      it))
		(end-hops (if (consp it)
			      (cdr it)
			    0)))
	    (forward-sexp (1+ start-hops))
	    (backward-sexp)
	    (setq start (point))
	    (forward-sexp (1+ end-hops))
	    (setq end (point))
	    (setf (point) start)
	    (when (looking-at "(") (forward-char))))
	(list start end)))))

(defun make-buffer-object (buffer)
  (with-current-buffer (get-buffer buffer)
    (let* ((buffer-contents
	    (buffer-substring-no-properties
	     (point-min)
	     (point-max)))
	   (buffer-sexp
	    (read (concat "(" buffer-contents ")"))))
      (make-object
       :buffer (current-buffer)
       :path nil
       :value buffer-sexp))))

(defun get-sub-objects (object-or-list filter)
  (if (listp object-or-list)
      (--mapcat
       (get-sub-objects-internal it filter)
       object-or-list)
    (get-sub-objects-internal object-or-list filter)))

(defun get-sub-objects-internal (object filter)
  (let ((next (object-value object))
	(current nil)
	(skip 0)
	(number 0)
	(result))
    (while next
      (setq current (car next))
      (setq next (cdr next))
      (let ((filter-action (funcall filter current next)))
	(when filter-action
	  (when (integerp filter-action)
	    (setq skip filter-action))
	  (push
	   (make-object
	    :buffer (object-buffer object)
	    :path (cons (if (= skip 0)
			    number
			  (cons number skip))
			(object-path object))
	    :value (if (= skip 0)
		       current
		     (cons current (-take skip next))))
	   result)))
      (setq number (1+ number)))
    (nreverse result)))

(defun get-object-location (object)
  (with-current-buffer (object-buffer object)
    (get-path-in-buffer (object-path object))))

(defun set-object-value (object value)
  (with-current-buffer (object-buffer object)
    (cl-destructuring-bind (start end)
	(get-object-location object)
      (delete-region start end)
      (save-excursion
	(setf (point) start)
	(insert (pp value))))))

(cl-defstruct object
  buffer
  path
  value)

(provide 'fi)
