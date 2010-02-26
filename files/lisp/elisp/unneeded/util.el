;;;
(defun ipr-meml (obj list)
  (delq 'klfsfff (mapcar '(lambda (elt) (if (equal obj elt) elt 'klfsfff))
			 list)))

(defun ipr-del-l (obj list)
  (delq 'klfsfff (mapcar '(lambda (elt) (if (equal obj elt) 'klfsfff elt))
			 list)))

(defun ipr-mem-if (fn list) 
  (delq 'klfsff (mapcar '(lambda (elt) (if (funcall fn elt) elt 'klfsff))
			list)))

(defun append-string-to-buffer (buffer string)
  (save-excursion
    (set-buffer (get-buffer-create buffer))
    (goto-char (point-max))
    (insert string)))

(defun ipr-setp-l (list)
  (not (memq 'klfsfff 
	     (mapcar '(lambda (elt)
			(if (ipr-meml elt (cdr (memq elt list))) 'klfsfff t))
		     list))))

