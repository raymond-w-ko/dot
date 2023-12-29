;;; rko-lib --- Summary -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(defun rko:update-alist (key value alist)
  "Update ALIST by setting the cdr of the pair identified by KEY to VALUE.
If KEY is not found in ALIST, add (KEY . VALUE) to the alist."
  (let ((pair (assoc key alist)))
    (if pair
        (setcdr pair value)  ; If the key exists, update the value
      (setq alist (cons (cons key value) alist)))  ; Otherwise, add a new key-value pair
    alist))

(provide 'rko-lib)
;;; rko-lib.el ends here
