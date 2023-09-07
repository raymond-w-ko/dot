;; -*- lexical-binding: t -*-

(setq-default show-trailing-whitespace nil)

(setq standard-indent 2)
(setq-default tab-width 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)

(setq completion-cycle-threshold 3)
(setq tab-always-indent 'complete)

;; is this a good idea?
(setq enable-recursive-minibuffers t)

(setq enable-remote-dir-locals t)

(require 'autorevert)
;; (setq global-auto-revert-non-file-buffers t)
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
  (setq tramp-use-ssh-controlmaster-options t)
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath=tramp.%%C -o ControlPersist=8h")
  (setq tramp-verbose 3)
  (setq vc-handled-backends '(Git SVN))
  (setq remote-file-name-inhibit-locks t)
  (setq tramp-histfile-override nil))

(require 'custom)
(require 'tramp)
(require 'vagrant-tramp nil t)
(require 'docker-tramp nil t)

;; this generally fixes staging chunks over tramp in magit
(defun rko/tramp-send-command--workaround-stty-icanon-bug (conn-vec orig-command &rest args)
  "See: https://github.com/magit/magit/issues/4720"
  (let ((command
         (if (string= "stty -icrnl -icanon min 1 time 0" orig-command)
             "stty -icrnl"
           orig-command)))
    (append (list conn-vec command) args)))

(defun rko/tramp-send-command--workaround-stty-icanon-bug--filter-args (args)
  (apply #'rko/tramp-send-command--workaround-stty-icanon-bug args))

(advice-add 'tramp-send-command :filter-args
            #'rko/tramp-send-command--workaround-stty-icanon-bug--filter-args)

(use-package eglot
  :straight (eglot :type git :host github :repo "joaotavora/eglot")
  :ensure t
  :init
  (setq eglot-connect-timeout 300))

(provide 'rko-emacs-builtin)
