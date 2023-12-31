;;; rko-emacs-keys --  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'rko-lib)

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

(use-package buffer-move
  :straight (buffer-move :host github :repo "lukhas/buffer-move"))

(use-package hop
  :straight (hop :host github :repo "Animeshz/hop.el")
  :init
  (setq hop-all-windows nil)
  ;; (setq hop-jump-keys (concat "uoemk"
  ;; "fhtns"
  ;; "ir"
  ;; "aljy"
  ;; "cdvp"
  ;; (reverse "zqxgwb")))
  (setq hop-jump-keys "asdghklqwertyuiopzxcvbnmfj")
  ;; :bind (("C-t" . hop-word))
  :init
  nil)

(use-package xah-fly-keys
  :straight (xah-fly-keys :host github :repo "xahlee/xah-fly-keys")
  :after (copilot buffer-move magit undo-fu)
  :init
  (setq xah-fly-use-meta-key t) ;; setting t clears most M-x bindings
  (setq xah-fly-use-control-key nil) ;; setting nil prevents adding C-x bindings
  :config
  (setq xah-fly-layouts (rko:update-alist "cc1" rko:xah-fly-cc1-layout xah-fly-layouts))
  (require 'rko-emacs-xah-fly-keys)
  (rko:switch-to-xah-cc1)
  nil)

(provide 'rko-emacs-keys)
;;; rko-emacs-keys.el ends here
