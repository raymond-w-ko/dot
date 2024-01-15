;;; init.el --- main entry point -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(when (daemonp)
  (princ "don't run emacs as a daemon\n" 'external-debugging-output)
  (kill-emacs 1))

(defvar rko-emacs-include-path-env-var)

(require 'cl-lib)
(require 'url-history)
(setq url-history-file (expand-file-name "url/history" user-emacs-directory))

(dolist (path '("rko-lisp" "rko-emacs-modules"))
  (add-to-list 'load-path (concat "~/dot/.emacs.d/" path)))
(cl-loop for file in '("/usr/bin/bash" "/bin/bash")
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

(use-package org
  :straight t
  :init
  (setq org-hide-emphasis-markers t)
  :config
  nil)

(use-package ef-themes
  :disabled
  :straight t
  :init
  (setq ef-themes-headings ; read the manual's entry or the doc string
        '((0 variable-pitch light 1.75)
          (1 variable-pitch light 1.4)
          (2 variable-pitch regular 1.2)
          (3 variable-pitch regular 1.1)
          (4 variable-pitch bold 1.0)
          (5 variable-pitch semibold 1.0)
          (6 variable-pitch regular 1.0)
          (7 variable-pitch regular 1.0)
          (t variable-pitch 1.0)))
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)
  (setq ef-themes-region '(intense no-extend neutral))
  (mapc #'disable-theme custom-enabled-themes)
  :config
  (ef-themes-select 'ef-melissa-light))

(setq use-package-verbose t)
(use-package solarized-theme
  :straight (solarized-emacs :type git :host github :repo "bbatsov/solarized-emacs")
  :ensure t
  :init
  (setq x-underline-at-descent-line t)
  (setq solarized-use-less-bold t)
  nil
  :config
  (load-theme 'solarized-selenized-dark t)
  nil)

;; pure utility packages
(use-package dash :straight t :ensure t)
(use-package s :straight t :ensure t)
(use-package f :straight t :ensure t)
(eval
 `(use-package pcre
    :straight (pcre :host github :repo "syohex/emacs-pcre"
                    :pre-build ("make" ,rko-emacs-include-path-env-var "all")
                    :files (:defaults "pcre.el" "pcre-core.so"))))

(use-package no-littering
  :straight t
  :demand t
  :config
  (no-littering-theme-backups))
;; not necessary with modern modelines
;; (use-package diminish :straight t)

(require 'rko-emacs-builtin)
(require 'rko-emacs-clipboard)

(load custom-file t)

(use-package spell-fu :straight t :ensure t)

(require 'rko-emacs-undo)
(require 'rko-emacs-project)
(require 'rko-emacs-modes)
(require 'rko-emacs-completion)
(require 'rko-emacs-devel)
(require 'rko-emacs-ui)
;; load everything before modifying keys
(require 'rko-emacs-keys)
;; these are more like fancy macros, which may assume keys are setup
(require 'rko-interactives)

(require 'desktop)
(defun rko/save-desktop ()
  "Save the desktop."
  (interactive)
  (message "Saving desktop...")
  (desktop-save user-emacs-directory t))
(defun rko/save-desktop-on-exit ()
  "Save the desktop on exit."
  (rko/save-desktop))
;; (add-hook 'kill-emacs-hook #'rko/save-desktop-on-exit 100)
(setq desktop-buffers-not-to-save "^$")
(setq desktop-files-not-to-save "^$")
(setq desktop-save t)
(setq desktop-load-locked-desktop t)
(setq desktop-path `(,user-emacs-directory))
;; (desktop-save-mode 1)

(toggle-frame-maximized)

(require 'server)
(unless (server-running-p) (server-start))

(provide 'init)
;;; init.el ends here
