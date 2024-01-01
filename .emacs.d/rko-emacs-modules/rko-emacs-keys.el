;;; rko-emacs-keys --  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'rko-lib)

(use-package buffer-move
  :straight (buffer-move :host github :repo "lukhas/buffer-move"))

(use-package hop
  :straight (hop :host github :repo "Animeshz/hop.el")
  :init
  (setq hop-all-windows nil)
  ;; (setq hop-jump-keys "asdghklqwertyuiopzxcvbnmfj")
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
