;;; rko-emacs-builtin.el --- builtin feature customization -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'rx)
(require 'js)
(require 'css-mode)
(require 'savehist)

(setq standard-indent 2)
(setq-default tab-width 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)
(defun rko/c-mode-common-case-fixer ()
  "Fix case indentation in `c-mode-common-hook'."
  (c-set-offset 'case-label '+))
(add-hook 'c-mode-common-hook 'rko/c-mode-common-case-fixer)
(defun rko/js-mode-case-fixer ()
  "Fix case indentation in `js-mode-hook' to match prettier indentation."
  (setq js-switch-indent-offset js-indent-level))
(add-hook 'js-mode-hook 'rko/js-mode-case-fixer)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package recentf
  :init
  (setq recentf-max-saved-items 10000
        recentf-max-menu-items 5000)
  (recentf-mode 1)
  (unless (boundp 'rko/recentf-save-list-timer-initialized)
    (defvar rko/recentf-save-list-timer-initialized t)
    (run-at-time nil (* 5 60) 'recentf-save-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dired)
(require 'dired-x)

(setq dired-omit-files (rx (or (seq bol "." eol)
                               (seq bol (one-or-more nonl) "~" eol)
                               (seq bol "#" (one-or-more nonl) "#" eol))))
(add-hook 'dired-mode-hook 'dired-omit-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rko/print-url-in-messages (url &rest args)
  "Print URL in *Messages* buffer instead of browsing it.
ARGS is ignored as URL is just echoed to *Messages* buffer."
  (message "URL: %s" url)
  (message "URL args: %s" args))

(setq browse-url-rowser-function 'rko/print-url-in-messages)

(require 'xref)
(setq xref-search-program 'ripgrep)
(setf (alist-get 'ripgrep xref-search-program-alist)
      "xargs -0 rg <C> --null -nH --no-heading --no-messages -e <R>")

(require 'tramp)
(require 'tramp-sh)
(setq tramp-verbose 4)
(setq vc-handled-backends '(Git SVN))
(setq remote-file-name-inhibit-locks t) ;; disables 100+ remote gio processes
;; t does not work well with eglot, causes failure to find shell prompt of /bin/sh ?
(setq tramp-use-ssh-controlmaster-options nil)
(setq tramp-ssh-controlmaster-options (concat "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
                                              "-o ControlMaster=auto -o ControlPersist=yes"))
(setq tramp-histfile-override nil)

(require 'custom)
(require 'tramp)
(require 'vagrant-tramp nil t)
(require 'docker-tramp nil t)

;; this generally fixes staging chunks over tramp in magit
(defun rko/tramp-send-command--workaround-stty-icanon-bug (conn-vec orig-command &rest args)
  "See: https://github.com/magit/magit/issues/4720 for an explanation.
CONN-VEC passes through.
ORIG-COMMAND passes through.
ARGS passes through."
  (let ((command
         (if (string= "stty -icrnl -icanon min 1 time 0" orig-command)
             "stty -icrnl"
           orig-command)))
    (append (list conn-vec command) args)))

(defun rko/tramp-send-command--workaround-stty-icanon-bug--filter-args (args)
  "Helper function for `rko/tramp-send-command--workaround-stty-icanon-bug'.
ARGS passes through."
  (apply #'rko/tramp-send-command--workaround-stty-icanon-bug args))

(advice-add 'tramp-send-command :filter-args
            #'rko/tramp-send-command--workaround-stty-icanon-bug--filter-args)

(use-package yasnippet
  :straight (yasnippet :type git :host github :repo "joaotavora/yasnippet")
  :ensure t
  :init
  (setq yas-overlay-priority 99)
  :config
  nil)

(use-package jsonrpc
  :straight t)

;; built-in for emacs 29 works better?
(use-package eglot
  :straight (eglot :type git :host github :repo "joaotavora/eglot")
  :after (jsonrpc)
  :init
  (setq eglot-sync-connect 15)
  (setq eglot-connect-timeout 300)
  nil)

(provide 'rko-emacs-builtin)
;;; rko-emacs-builtin.el ends here
