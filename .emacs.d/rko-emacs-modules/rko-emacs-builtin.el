;; -*- lexical-binding: t -*-

(setq-default tab-width 2)
(setq js-indent-level 2)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)

(require 'autorevert)
(setq global-auto-revert-non-file-buffers t)
(add-to-list 'global-auto-revert-ignore-modes 'buffer-menu-mode)
(add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)
(global-auto-revert-mode 1)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(when (fboundp 'windmove-default-keybindings) (windmove-default-keybindings))

;; tab-bar
(setq tab-bar-show t)
(setq tab-bar-close-button-show nil)
(setq tab-bar-tab-hints t)
(setq tab-bar-new-tab-choice "*scratch*")
(setq tab-bar-format '(tab-bar-format-history
                       tab-bar-format-tabs
                       tab-bar-separator))
(tab-bar-mode 1)

(transient-mark-mode 1)

(set-fringe-mode 0)
(electric-pair-mode 1)
(winner-mode 1)
(recentf-mode 1)
;; (pixel-scroll-mode 1)

(setq history-length 256)
(setq savehist-additional-variables '())
(add-to-list 'savehist-additional-variables 'register-alist)
(add-to-list 'savehist-additional-variables 'kill-ring)
(savehist-mode 1)

;; (save-place-mode 1)

(defun rko/print-url-in-messages (url &rest args)
  "Print URL in *Messages* buffer instead of browsing it."
  (message "URL: %s" url))

(setq browse-url-browser-function 'rko/print-url-in-messages)

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

(setq auto-revert-remote-files t)

;; (use-package tramp :straight t :defer t)
(use-package eglot :straight t :ensure t)

(provide 'rko-emacs-builtin)
