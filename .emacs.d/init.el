;; -*- lexical-binding: t -*-

(when (daemonp)
  (princ "don't run emacs as a daemon\n" 'external-debugging-output)
  (kill-emacs 1))

(require 'cl-lib)

(dolist (path '("rko-lisp" "rko-emacs-modules"))
  (add-to-list 'load-path (concat "~/dot/.emacs.d/" path)))
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

(use-package ef-themes
  :straight t
  :init
  (setq ef-themes-headings ; read the manual's entry or the doc string
        '((0 variable-pitch light 1.6)
          (1 variable-pitch light 1.5)
          (2 variable-pitch regular 1.4)
          (3 variable-pitch regular 1.3)
          (4 variable-pitch regular 1.2)
          (5 variable-pitch 1.1)    ; absence of weight means `bold'
          (6 variable-pitch 1.0)
          (7 variable-pitch 1.0)
          (t variable-pitch 1.0)))
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)
  (setq ef-themes-region '(intense no-extend neutral))
  (mapc #'disable-theme custom-enabled-themes)
  :config
  (ef-themes-select 'ef-elea-light))

;; pure utility packages
(use-package dash :straight t :ensure t)
(use-package s :straight t :ensure t)
(use-package f :straight t :ensure t)
(eval
 `(use-package pcre
    :straight (pcre :host github :repo "syohex/emacs-pcre"
                    :pre-build ("make" ,rko-emacs-include-path-env-var "all")
                    :files (:defaults "pcre.el" "pcre-core.so"))))

(use-package no-littering :straight t)
;; not necessary with modern modelines
;; (use-package diminish :straight t)

(require 'rko-emacs-builtin)
(require 'rko-emacs-clipboard)

(load custom-file t)

(use-package spell-fu :straight t :ensure t)

(require 'rko-emacs-undo)
(require 'rko-emacs-keys)
(require 'rko-emacs-project)
(require 'rko-emacs-modes)
(require 'rko-emacs-completion)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package buffer-move
  :straight (buffer-move :host github :repo "lukhas/buffer-move")
  :bind (("C-S-<up>" . buf-move-up)
         ("C-S-<down>" . buf-move-down)
         ("C-S-<left>" . buf-move-left)
         ("C-S-<right>" . buf-move-right)

         ("C-S-i" . buf-move-up)
         ("C-S-k" . buf-move-down)
         ("C-S-j" . buf-move-left)
         ("C-S-l" . buf-move-right)))

(use-package super-save
  :straight t
  :diminish super-save-mode
  :config
  ;; At this point you can probably switch off the built-in auto-save-mode
  ;; (unless you really care about its backups) 
  (setq auto-save-default nil)
  (super-save-mode +1)
  ;; this may cause issues with tramp because of timers
  (setq super-save-auto-save-when-idle nil))

(use-package magit
  :straight (magit :host github :repo "magit/magit")
  :init
  nil
  :config
  nil)

(use-package diff-hl
  :straight t
  :config
  (global-diff-hl-mode -1))

(setq vterm-always-compile-module t)
(use-package vterm :straight t
  :init
  (setq vterm-shell shell-file-name)
  (setq vterm-tramp-shells '(("docker" "sh")
                             ("ssh" "'zsh'"))))
(use-package multi-vterm :straight t)

(when (executable-find "dtach")
  (use-package detached
    :straight (detached :type git :host nil :repo "https://git.sr.ht/~niklaseklund/detached.el"
                        :fork (:host nil :repo "git@github.com:raymond-w-ko/detached.el"))
    :init
    (setq detached-degraded-commands '("^ls"))
    (detached-init)
    :bind (;; Replace `async-shell-command' with `detached-shell-command'
           ([remap async-shell-command] . detached-shell-command)
           ;; Replace `compile' with `detached-compile'
           ([remap compile] . detached-compile)
           ([remap recompile] . detached-compile-recompile)
           ;; Replace built in completion of sessions with `consult'
           ([remap detached-open-session] . detached-consult-session))
    :custom ((detached-terminal-data-command system-type))))

(connection-local-set-profile-variables
 'remote-detached
 '((detached-shell-program . "/bin/bash")
   (detached-session-directory . "~/dtach-sessions")
   (detached-dtach-program . "dtach")))

(connection-local-set-profiles
 '(:application tramp :protocol "ssh") 'remote-detached)

(use-package rg
  :straight (rg :host github :repo "dajva/rg.el")
  :config
  (rg-enable-default-bindings))

(require 'rko-emacs-ui)
(require 'rko-interactives)

(defun rko/save-desktop ()
  (interactive)
  (message "Saving desktop...")
  (desktop-save user-emacs-directory t))
(defun rko/save-desktop-on-exit ()
  (rko/save-desktop))
;; (add-hook 'kill-emacs-hook #'rko/save-desktop-on-exit 100)
(setq desktop-buffers-not-to-save "^$")
(setq desktop-files-not-to-save "^$")
(setq desktop-save t)
(setq desktop-load-locked-desktop t)
(setq desktop-path `(,user-emacs-directory))
;; (desktop-save-mode 1)

(toggle-frame-maximized)
