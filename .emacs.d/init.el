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

;; emacs
(set-default-coding-systems 'utf-8)

;; make emacs not store in a version controlled directory
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))
(make-directory user-emacs-directory t)
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "eln-cache" user-emacs-directory)))
(use-package no-littering :straight t)

(unless (boundp 'rko-init)
  (push "~/dot/.emacs.d/lisp" load-path)
  (setq rko-init t))

(setq line-spacing nil)
(setq inhibit-startup-screen t)
(setq initial-buffer-choice t)
(setq visible-bell t)

(setq gc-cons-threshold (* 100 1000 1000))
(setq read-process-output-max (* 1024 1024))

(setq-default indent-tabs-mode nil)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(electric-pair-mode 1)
(pixel-scroll-mode 1)

;; minor packages
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
  :straight t
  :bind
  (("M-o" . ace-window))
  :init
  (setq aw-dispatch-always t)
  :config
  (ace-window-display-mode 1))

(use-package avy
  :straight t
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char)
  (global-set-key (kbd "C-'") 'avy-goto-char-2))

(use-package company
  :straight t
  :hook ((after-init . global-company-mode)))

(use-package magit
  :straight t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Term" :foundry "UKWN" :slant normal :weight regular :height 98 :width normal)))))

