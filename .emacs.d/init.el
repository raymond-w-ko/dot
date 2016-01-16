(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; normalize directories in case symlinks are used
(defvar emacs-d
  (file-name-directory
   (file-chase-links load-file-name)))

;; backups and auto-save go in their own directory
(defvar emacs-backup-directory (expand-file-name "backup/"))
(unless (file-exists-p emacs-backup-directory)
  (make-directory emacs-backup-directory))
(setq backup-directory-alist
      `((".*" . ,emacs-backup-directory)))

(defvar emacs-auto-save-directory (expand-file-name "auto-save/"))
(unless (file-exists-p emacs-auto-save-directory)
  (make-directory emacs-auto-save-directory))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-auto-save-directory t)))

;; disable lock files, everything is version controlled, or no one
;; edits the same files I am using.
(setq create-lockfiles nil)

(require 'package)
(setq package-user-dir (expand-file-name "elpa" emacs-d))
;; https://github.com/milkypostman/melpa#usage
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; stolen from https://github.com/Lokaltog/home/blob/develop/home/.emacs.d/init.el
(defun rko-require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE"
  (if (package-installed-p package min-version) t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (rko-require-package package min-version t)))))

(rko-require-package 'use-package)
(require 'use-package)

(setq-default
 inhibit-splash-screen t
 inhibit-startup-message t
 initial-scratch-message nil

 ;; warn when opening files bigger than 100 MB
 large-file-warning-threshold 100000000

 ;; http://www.emacswiki.org/emacs/SmoothScrolling
 scroll-margin 10
 scroll-step 1
 scroll-conservatively 100000
 auto-window-vscroll nil

 indent-tabs-mode nil
 tab-width 2
 require-final-newline t

 indicate-empty-lines t
 indicate-buffer-boundaries '((top . left) (bottom . left) (t . right))

 split-height-threshold nil
 split-width-threshold 0

 major-mode 'text-mode
 fill-colum 80
 x-stretch-cursor t

 gc-cons-threshold 20000000
 blink-cursor-alist '((box . hbar))

 echo-keystrokes 0.02

 custom-file (expand-file-name "init-custom.el" emacs-d))

(fset 'yes-or-no-p 'y-or-n-p)

;; hide all UI
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; global modes
(electric-indent-mode t)
(transient-mark-mode t)
(blink-cursor-mode t)
(column-number-mode t)
(line-number-mode t)

;; hooks
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(defun rko-font-lock-comment-annotations ()
  "Hilight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):?"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'rko-font-lock-comment-annotations)

(use-package diminish
  :ensure t)

(use-package server
  :if window-system
  :config
  (unless (server-running-p)
    (server-start)))

(use-package distinguished-theme
  :ensure t
  :config
  (load-theme 'distinguished t))

(use-package popwin
  :commands popwin-mode
  :config
  (popwin-mode 1)
  (push '("*Compile-Log*" :height 20 :noselect t) popwin:special-display-config))

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (global-company-mode))

(defvar my-packages
  '(evil
    evil-leader
    evil-tabs
    evil-paredit
    evil-surround
    evil-escape
    smooth-scrolling
    paredit
    helm
    helm-projectile
    key-chord
    lispy
    avy
    ace-window
    projectile
    color-theme-solarized))
(dolist (package my-packages)
  (unless (package-installed-p package)
    (ignore-errors (package-install package))))
(defun my-upgrade-packages ()
  (save-window-excursion
    (package-list-packages t)
    (package-menu-mark-upgrades)
    (condition-case nil
        (package-menu-execute t)
      (error
       (package-menu-execute)))))

(add-to-list 'load-path (expand-file-name "lisp/" emacs-d))
(require 'rko-ins)

(require 'projectile)
(projectile-global-mode)

(require 'helm)
(require 'helm-config)
(helm-mode 1)

(setq projectile-completion-system 'helm)
(helm-projectile-on)

(add-hook 'emacs-lisp-mode-hook
	  (lambda () (lispy-mode 1)))

(global-set-key (kbd "M-p") 'ace-window)
(global-set-key (kbd "π") 'avy-goto-char)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "θ") 'rko-quotes)               ; q
(global-set-key (kbd "χ") 'lispy-right)              ; x
(global-set-key (kbd "φ") 'rko-parens) 	             ; f

(setq show-paren-delay 0)

;; (setq key-chord-two-keys-delay 0.1)
;; (key-chord-mode 1)

(blink-cursor-mode 0)
(global-hl-line-mode 1)

;; (require 'uniquify)
;; (setq
;;  uniquify-buffer-name-style 'post-forward
;;  uniquify-separator ":")

(winner-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(frame-background-mode (quote dark)))

(if (eq system-type 'windows-nt)
    (progn (set-face-attribute 'default nil :font "Consolas 8")
           (set-frame-font "Consolas 8" nil t)))

(load-theme 'solarized t)
