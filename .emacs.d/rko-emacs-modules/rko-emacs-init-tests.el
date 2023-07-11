;; -*- lexical-binding: t -*-

(defun rko--test-pcre ()
  (require 'pcre)
  
  (let ((str "012-345-567"))
    (when (pcre-string-match "\\A(\\d+)-(\\d+)-(\\d+)\\z" str)
      (match-string 1 str)))

  (with-temp-buffer
    (insert "apple orange melon\n")
    (insert "red blue green\n")
    (insert "vim atom sublime\n")
    (goto-char (point-min))
    (let (matches)
      (while (pcre-re-search-forward "^\\S+ ([^[:space:]]+)" nil t)
        (push (match-string 1) matches))
      (reverse matches)))
  
  nil)

(provide 'rko-emacs-inits-tests)
