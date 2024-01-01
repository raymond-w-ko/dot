;;; rko-lib --- Summary -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(defconst rko:dvorak-to-qwerty
  '("1234567890[]',.pyfgcrl/=aoeuidhtns-;qjkxbmwvz" .
    "1234567890-=qwertyuiop[]asdfghjkl;'zxcvbnm,./"))
(defconst rko:qwerty-to-dvorak
  (cons (cdr rko:dvorak-to-qwerty)
        (car rko:dvorak-to-qwerty)))

(defconst rko:dvorak-to-cc1
  '("1234567890[]',.pyfgcrl/=aoeuidhtns-;qjkxbmwvz" .
    "1234567890[]q.gwrylpjx/=muoezatds;,ikcv'nf-hb"))

(defconst rko:xah-fly-cc1-layout
  (let ((pairs '()))
    (dotimes (i (length (car rko:dvorak-to-qwerty)))
      (let ((dvorak (aref (car rko:dvorak-to-qwerty) i))
            (cc1 (aref (cdr rko:dvorak-to-cc1) i)))
        (push (cons (char-to-string dvorak) (char-to-string cc1)) pairs)))
    (reverse pairs)))

(defconst rko:qwerty-to-cc1-ht
  (let ((ht (make-hash-table :test 'equal)))
    (dotimes (i (length (car rko:dvorak-to-qwerty)))
      (let ((qwerty (aref (cdr rko:dvorak-to-qwerty) i))
            (cc1 (aref (cdr rko:dvorak-to-cc1) i)))
        (puthash qwerty cc1 ht)))
    ht))

(defconst rko:qwerty-hop-keys "asdghklqwertyuiopzxcvbnmfj")
(defconst rko:cc1-hop-keys
  (with-output-to-string
    (dotimes (i (length rko:qwerty-hop-keys))
      (let* ((qwerty (aref rko:qwerty-hop-keys i))
             (cc1 (gethash qwerty rko:qwerty-to-cc1-ht)))
        (princ (char-to-string cc1))))))

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
