;; settings needed before evil loads
(setq evil-move-cursor-back t
      evil-cross-lines t
      evil-want-fine-undo t
      evil-want-C-u-scroll t
      evil-want-C-w-in-emacs-state nil
      evil-want-C-w-delete t)

(require 'evil)
(evil-mode 1)

(setq-default evil-escape-delay 0.1)
(setq-default evil-escape-key-sequence "fj")
(evil-escape-mode)

(provide 'evil-settings)
