;;; rko-emacs-init-tests --- -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'pcre)

(defun rko--test-pcre ()
  "Test \\='pcre\\=' package."

  (let ((str "012-345-567"))
    (when (pcre-string-match "\\A(\\d+)-(\\d+)-(\\d+)\\z" str)
      (message "%s" (match-string 1 str))))

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

(provide 'rko-emacs-init-tests)
;;; rko-emacs-init-tests.el ends here
