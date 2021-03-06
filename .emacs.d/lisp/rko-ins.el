(require 'lispy)

(defun rko-quotes (arg)
  (interactive "P")
  (cond ((eq major-mode 'term-mode)
	 (term-send-raw-string "\"\"")
	 (term-send-raw-string ""))

	((region-active-p)
	 (let ((beg (region-beginning))
	       (end (region-end)))
	   (deactivate-mark)
	   (goto-char beg)
	   (insert "\"")
	   (goto-char (1+ end))
	   (insert "\"")))

	((and (not (eq major-mode 'org-mode))
	      (lispy--in-string-p)
	      (not arg))
	 (insert "\\\"\\\"")
	 (backward-char 2))

	((and (eq (char-before) ?<)
	      (eq (char-after) ?>))
	 (delete-region (1- (point))
			(1+ (point)))
	 (insert "\"\"")
	 (backward-char))

	(t
	 (insert "\"\"")
	 (backward-char))))

(defun rko-single-quotes (arg)
  (interactive "P")
  (if (region-active-p)
      (lispy--surround-region "'" "'")
    (insert "''")
    (backward-char)))

(defun rko-parens ()
  (interactive)
  (cond ((eq major-mode 'term-mode)
	 (term-send-raw-string "()")
	 (term-send-raw-string ""))
	((region-active-p)
	 (lispy--surround-region "(" ")"))
	(t
	 (if (looking-back "\\(if\\)\\|\\(for\\)\\|\\(switch\\)\\|\\(while\\)")
	     (insert " "))
	 (insert "()")
	 (backward-char))))

(provide 'rko-ins)
