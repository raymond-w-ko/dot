;; emacs
(setq gc-cons-threshold (* 100 1000 1000))
(set-default-coding-systems 'utf-8)
(setq read-process-output-max (* 1024 1024))

;; make emacs not store in a version controlled directory
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))
(make-directory user-emacs-directory t)
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "eln-cache" user-emacs-directory)))

;; package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(require 'straight-x)

(use-package no-littering :straight t)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
;; (load custom-file t)

(unless (boundp 'rko/init)
  (push "~/.emacs.d/lisp" load-path)
  (setq rko/init t))

(setq line-spacing nil)
(setq inhibit-startup-screen t)
(setq initial-buffer-choice t)
(setq visible-bell t)

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)

(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(when (fboundp 'windmove-default-keybindings) (windmove-default-keybindings))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(set-fringe-mode 10)
(electric-pair-mode 1)
;; (pixel-scroll-mode 1)
(winner-mode 1)

(defun rko/print-url-in-messages (url &rest args)
  "Print URL in *Messages* buffer instead of browsing it."
  (message "URL: %s" url))

(setq browse-url-browser-function 'rko/print-url-in-messages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package diminish :straight t)

(use-package which-key
  :straight t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1.0))

;; devil
(use-package devil
  :straight t
  :init
  (setq devil-lighter " \U0001F608")
  (setq devil-prompt "\U0001F608 %t")
  :config
  (global-devil-mode)
  (global-set-key (kbd "C-,") 'global-devil-mode))

(use-package lispy
  :straight t
  :hook ((emacs-lisp-mode . (lambda () (lispy-mode 1)))))

;; ace window
(use-package ace-window
  :disabled
  :straight t
  :bind
  (("M-o" . ace-window))
  :init
  (setq aw-dispatch-always t)
  :config
  (ace-window-display-mode 1))

(use-package avy
  :straight t
  :init
  (setq avy-style 'words)
  (setq avy-background t)
  :bind (("C-:" . avy-goto-char-2))
  :config
  (avy-setup-default))

(use-package avy-zap
  :straight t
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

(use-package savehist
  :straight t
  :init
  (savehist-mode))

;; (use-package company
;;   :straight t
;;   :hook ((after-init . global-company-mode)))

(use-package vertico
  :straight t
  :custom (vertico-cycle t)
  :init
  (vertico-mode))

(use-package orderless
  :straight t
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :config
  (global-corfu-mode))

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :hook (prog-mode . copilot-mode)
  :bind (("C-c M-f" . copilot-complete)
         :map copilot-completion-map
         ("C-g" . 'copilot-clear-overlay)
         ("M-p" . 'copilot-previous-completion)
         ("M-n" . 'copilot-next-completion)
         ("<tab>" . 'copilot-accept-completion)
         ("M-f" . 'copilot-accept-completion-by-word)
         ("M-<return>" . 'copilot-accept-completion-by-line))
  :config
  (require 'copilot))

(use-package magit
  :straight t)

(use-package zoom
  :straight t
  :diminish zoom-mode
  :init (custom-set-variables '(zoom-size '(0.618 . 0.618)))
  :config (zoom-mode 1))

(use-package vterm
  :straight t)

;; (use-package emojify
;;   :hook (erc-mode . emojify-mode)
;;   :commands emojify-mode)

;; theme
(use-package zenburn-theme :straight t :defer t)
(use-package moe-theme :straight t :defer t)
(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-gruvbox-light t)
  (doom-themes-visual-bell-config))

;; The quick brown fox jumped over the lazy dog
(setq rko/font-face-height 86)
(set-face-attribute 'default nil
                    :font "Iosevka Term"
                    :weight 'light
                    :height rko/font-face-height)
(set-face-attribute 'fixed-pitch nil
                    :font "Iosevka Term"
                    :weight 'light
                    :height rko/font-face-height)
(set-face-attribute 'variable-pitch nil
                    :font "Iosevka Aile"
                    :weight 'light
                    :height rko/font-face-height)
