;; -*- lexical-binding: t -*-

(setq-default show-trailing-whitespace nil)
(setq-default tab-width 2)
(setq js-indent-level 2)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)
(setq completion-cycle-threshold 3)
(setq tab-always-indent 'complete)
;; is this a good idea?
(setq enable-recursive-minibuffers t)

(require 'autorevert)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-remote-files t)
(add-to-list 'global-auto-revert-ignore-modes 'buffer-menu-mode)
(add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)
;; (global-auto-revert-mode 1)

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

(fringe-mode 8)
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

(require 'xref)
(setq xref-search-program 'ripgrep)
(setf (alist-get 'ripgrep xref-search-program-alist)
      "xargs -0 rg <C> --null -nH --no-heading --no-messages -e <R>")

(use-package tramp
  :init
  (setq tramp-verbose 2))

(use-package eglot
  :straight (eglot :type git :host github :repo "joaotavora/eglot")
  :ensure t)

(provide 'rko-emacs-builtin)