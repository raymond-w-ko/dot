(require 'cl-lib)
(cl-loop for file in '("/bin/zsh" "/bin/bash")
         when (file-exists-p file)
         do (progn
              (setq shell-file-name file)
              (cl-return)))
(setenv "SHELL" shell-file-name)

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
  (push "~/.emacs.d/lisp" load-path))

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
(desktop-save-mode 1)
(winner-mode 1)
(recentf-mode 1)
(setq history-length 64)
(savehist-mode 1)
;; (save-place-mode 1)

(defun rko/print-url-in-messages (url &rest args)
  "Print URL in *Messages* buffer instead of browsing it."
  (message "URL: %s" url))

(setq browse-url-browser-function 'rko/print-url-in-messages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package diminish :straight t)

(defun rko/undo-tree-save-history (undo-tree-save-history &rest args)
  (let ((message-log-max nil)
        (inhibit-message t))
    (apply undo-tree-save-history args)))
(use-package undo-tree
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

(use-package super-save
  :straight t
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(use-package which-key
  :straight t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1.0)
  (setq which-key-popup-type 'minibuffer)
  (setq which-key-side-window-location 'right)
  (setq which-key-side-window-max-width 30)
  (setq which-key-side-window-max-height 0.20))

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

(use-package projectile
  :straight t
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

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
  :disabled
  :straight t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :config
  (global-corfu-mode))

(use-package spell-fu :straight t :ensure t)
(use-package mono-complete
  :straight t
  :ensure t
  :commands (mono-complete-mode)
  :hook ((prog-mode text-mode org-mode) . mono-complete-mode)
  :init
  (require 'spell-fu)
  (setq mono-complete-debug-log 'stdout)
  (setq mono-complete-backend-capf-complete-fn
        nil)
  (setq mono-complete-backends
        (lambda (is-context)
          (cond
           (is-context
            (let* ((result (list))
                   (state (syntax-ppss))
                   (is-string (nth 3 state))
                   (is-comment (nth 4 state)))
              (when (or is-string is-comment)
                (push 'filesystem result))
              (push 'capf result)
              (push 'dabbrev result)
              (push 'spell-fu result)
              result))
           (t
            (list 'capf 'dabbrev 'filesystem 'spell-fu)))))
  (setq mono-complete-fallback-command 'indent-for-tab-command)
  ;; this is required for it to work
  (setq mono-complete-evil-insert-mode-only nil)
  (custom-set-faces `(mono-complete-preview-face ((t :inherit font-lock-comment-face)) t))
  (setq completion-fail-discreetly t)
  :config
  (define-key mono-complete-mode-map (kbd "<tab>") 'mono-complete-expand-or-fallback))

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  ;; :hook (prog-mode . copilot-mode)
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
(use-package diff-hl
  :straight t
  :config
  (global-diff-hl-mode))

(use-package zoom
  :straight t
  :diminish zoom-mode
  :init (custom-set-variables '(zoom-size '(100 . 0.618)))
  :config (zoom-mode 1))

(setq vterm-always-compile-module t)
(use-package vterm :straight t
  :init
  (setq vterm-shell shell-file-name))
(use-package multi-vterm :straight t)

;; (use-package emojify
;;   :hook (erc-mode . emojify-mode)
;;   :commands emojify-mode)

(use-package prism
  :straight (el-patch :type git :host github :repo "alphapapa/prism.el"))

(prism-set-colors :num 16
  :desaturations (cl-loop for i from 0 below 16
                          collect (* i 2.5))
  :lightens (cl-loop for i from 0 below 16
                     collect (* i 2.5))
  :colors (list "dodgerblue" "medium sea green" "sandy brown")

  :comments-fn
  (lambda (color)
    (prism-blend color
                 (face-attribute 'font-lock-comment-face :foreground) 0.25))

  :strings-fn
  (lambda (color)
    (prism-blend color "white" 0.5)))

(use-package dimmer
  :straight t
  :init
  (setq dimmer-fraction 0.33)
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  (dimmer-configure-org)
  (dimmer-configure-gnus)
  (dimmer-configure-helm)
  (dimmer-mode t))

;; theme
(use-package zenburn-theme :straight t :defer t)
(use-package moe-theme :straight t :defer t)
(use-package doom-themes
  :disabled
  :straight t
  :config
  (load-theme 'doom-gruvbox-light t)
  (doom-themes-visual-bell-config))

(use-package solarized-theme
  :straight t
  :config
  (load-theme 'solarized-dark t))

;; The quick brown fox jumped over the lazy dog
(setq rko/font-face-height (floor (* (/ 9.5 10.0) 96.0)))
(defun rko/set-fonts ()
  (dolist (x '(t nil))
    (set-face-attribute 'default x :font "Iosevka Term" :weight 'medium :height rko/font-face-height)
    (set-face-attribute 'fixed-pitch x :font "Iosevka Term" :weight 'medium :height rko/font-face-height)
    (set-face-attribute 'variable-pitch x :font "Iosevka Aile" :weight 'medium :height rko/font-face-height)))
(rko/set-fonts)
(setq rko/init t)
