;; -*- lexical-binding: t -*-

(load custom-file t)

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
(menu-bar-mode -1)
(blink-cursor-mode -1)
(set-fringe-mode 8)
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
(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

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
  :init
  (setq projectile-project-search-path '("~/dot/.emacs.d/"
                                         "~/src/"
                                         "~/.cache/emacs/straight/repos/"))
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; (use-package company
;;   :straight t
;;   :hook ((after-init . global-company-mode)))

(use-package orderless
  :straight t
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :straight t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  :config
  (setq consult-narrow-key "<"))

(use-package vertico
  :straight t
  :custom (vertico-cycle t)
  :init
  (require 'consult)
  (vertico-mode))

(use-package marginalia
  :straight t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package corfu
  :straight (corfu :type git :host github :repo "raymond-w-ko/corfu")
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :config
  (global-corfu-mode))

(use-package spell-fu :straight t :ensure t)
(use-package mono-complete
  :disabled
  :straight t
  :hook ((prog-mode text-mode org-mode) . mono-complete-mode)
  :init
  (require 'spell-fu)
  (setq mono-complete-debug-log t)
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

(use-package telephone-line
  :straight t
  :disabled
  :init
  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
        telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
        telephone-line-primary-right-separator 'telephone-line-cubed-right
        telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-minor-mode-segment
                     telephone-line-buffer-segment))))
  (setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (evil   . (telephone-line-airline-position-segment))))
  (setq telephone-line-height 18
        telephone-line-evil-use-sort-tag t)
  :config
  (telephone-line-mode 1))

(use-package smart-mode-line
  :straight t
  :init
  (setq sml/theme 'respectful)
  :config
  (add-to-list 'sml/replacer-regexp-list '("^~/dot/\\.emacs\\.d/" ":ED:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/src/" ":SRC:"))
  (sml/setup))

(use-package idle-highlight
  :straight t
  :hook ((prog-mode text-mode) . idle-highlight-mode)
  :init
  (setq idle-highlight-idle-time 1.0))

(use-package centaur-tabs
  :disabled
  :straight t
  :init
  (setq centaur-tabs-style "bar"
        centaur-tabs-height 22
        centaur-tabs-enable-key-bindings t
        centaur-tabs-set-bar 'under
        x-underline-at-descent-line t
        centaur-tabs--buffer-show-groups nil
        centaur-tabs-set-icons nil
        centaur-tabs-set-modified-marker t
        centaur-tabs-show-navigation-buttons nil
        centaur-tabs-enable-ido-completion nil
        centaur-tabs-adjust-buffer-order t)
  :config
  (centaur-tabs-mode t)
  (setq uniquify-separator "/")
  (setq uniquify-buffer-name-style 'forward)
  (centaur-tabs-headline-match)
  (centaur-tabs-change-fonts (face-attribute 'default :font) 90)
  (defun entaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.
Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \\"Emacs\\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
                          help-mode))
       "Help")
      ((memq major-mode '(org-mode
                          org-agenda-clockreport-mode
                          org-src-mode
                          org-agenda-mode
                          org-beamer-mode
                          org-indent-mode
                          org-bullets-mode
                          org-cdlatex-mode
                          org-agenda-log-mode
                          diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer)))))))

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

(defun rko/nop ()
  (prism-set-colors
    :save t
    :num 16
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
      (prism-blend color "white" 0.5))))

(defun rko/setup-post-frame-config (&optional frame)
  (use-package prism
    :straight (prism :type git :host github :repo "alphapapa/prism.el")
    :config)
  
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
    (dimmer-mode t)))

(setq rko/init t)
(add-hook 'after-make-frame-functions #'rko/setup-post-frame-config)
