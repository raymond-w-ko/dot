;; rko-emacs-undo --- -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(defun rko/undo-tree-save-history (undo-tree-save-history &rest args)
  "Advice to silence undo-tree-save-history."
  (let ((message-log-max nil)
        (inhibit-message t))
    (apply undo-tree-save-history args)))
(use-package undo-tree
  :disabled
  :straight t
  :diminish undo-tree-mode
  :init
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-auto-save-history t)
  (let ((undo-dir (expand-file-name "undo" user-emacs-directory)))
    (make-directory undo-dir t)
    (setq undo-tree-history-directory-alist `(("." . ,undo-dir))))
  :config
  (global-undo-tree-mode 1)
  (advice-add 'undo-tree-save-history :around 'rko/undo-tree-save-history))

(use-package undo-fu
  :straight t
  :init
  (setq undo-limit 67108864) ; 64mb.
  (setq undo-strong-limit 100663296) ; 96mb.
  (setq undo-outer-limit 1006632960) ; 960mb.
  :config
  nil)

(use-package undo-fu-session
  :straight t
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (setq undo-fu-session-incompatible-major-modes '())
  (undo-fu-session-global-mode))

(use-package vundo
  :straight t
  :init
  (setq vundo-compact-display nil)
  :config
  (setq vundo-glyph-alist '((selected-node . ?x)
                            (node . ?o)
                            (horizontal-stem . 9472)
                            (vertical-stem . 9474)
                            (branch . 9500)
                            (last-branch . 9492)))
  nil)

(provide 'rko-emacs-undo)
;;; rko-emacs-undo.el ends here
