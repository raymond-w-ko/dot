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
          (5 variable-pitch 1.1)      ; absence of weight means `bold'
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

(load custom-file t)

(use-package spell-fu :straight t :ensure t)

(require 'rko-emacs-undo)
(require 'rko-emacs-keys)
(require 'rko-emacs-project)
(require 'rko-emacs-modes)
(require 'rko-emacs-completion)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

(use-package super-save
  :straight t
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(use-package magit
  :straight t)
(use-package diff-hl
  :straight t
  :config
  (global-diff-hl-mode -1))

;; zoom and vundo do not play well
(use-package zoom
  :disabled
  :straight t
  :diminish zoom-mode
  :init
  (custom-set-variables
   '(zoom-size '(100 . 50))
   '(zoom-ignored-major-modes '(vundo-mode))
   '(zoom-ignored-buffer-names '(" *vundo tree*")))
  :config
  (zoom-mode 1))

(setq vterm-always-compile-module t)
(use-package vterm :straight t
  :init
  (setq vterm-shell shell-file-name)
  (setq vterm-tramp-shells '(("docker" "sh")
                             ("ssh" "'zsh'"))))
(use-package multi-vterm :straight t)

(when (executable-find "dtach")
  (use-package detached
    :straight t
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
    :custom ((detached-show-output-on-attach t)
             (detached-terminal-data-command system-type))))

(connection-local-set-profile-variables
 'remote-detached
 '((detached-shell-program . "/bin/bash")
   (detached-session-directory . "~/dtach-sessions")
   (detached-dtach-program . "dtach")))

(connection-local-set-profiles
 '(:application tramp :protocol "ssh") 'remote-detached)

(use-package pulsar
  :straight t
  :init
  (setq pulsar-face 'pulsar-green)
  (setq pulsar-pulse nil)
  :config
  (require 'pulsar)
  (pulsar-global-mode 1)
  (add-hook 'next-error-hook #'pulsar-pulse-line)
  (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
  (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry))

(use-package symbol-overlay
  :init
  (global-set-key (kbd "<f5>") 'symbol-overlay-put)
  (global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
  (global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
  (global-set-key (kbd "<f7>") 'symbol-overlay-mode)
  (global-set-key (kbd "<f8>") 'symbol-overlay-remove-all)
  :straight t)

(use-package nerd-icons
  :straight t
  :init
  (setq nerd-icons-scale-factor 1.00)
  :config
  (unless (file-exists-p "~/.local/share/fonts/NFM.ttf")
    (nerd-icons-install-fonts t)))
(use-package nerd-icons-dired
  :straight t
  :hook ((dired-mode) . nerd-icons-dired-mode))

(use-package minions
  :straight t)

(use-package doom-modeline
  :straight t
  :init
  (setq doom-modeline-height 1)
  (setq doom-modeline-time t)
  (setq doom-modeline-time-icon t)
  
  (setq doom-modeline-project-detection 'project)
  (setq doom-modeline-workspace-name t)
  (setq doom-modeline-persp-name t)
  
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-icon t)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-modal t)
  (setq doom-modeline-modal-icon t)
  :config
  (doom-modeline-mode 1))

(defun rko/setup-prism-for-dark-theme ()
  (prism-set-colors :num 16
    :save t
    :desaturations (cl-loop for i from 0 below 16
                            collect (* i 30))
    :lightens (cl-loop for i from 0 below 16
                       collect (* -1 i 10))
    :colors (list "dodgerblue" "medium sea green" "sandy brown"))
  nil)

(defun rko/setup-prism-for-light-theme ()
  (require 'prism)
  (setq prism-num-faces 16)
  (prism-set-colors
    :desaturations '(0) ; do not change---may lower the contrast ratio
    :lightens '(0)      ; same
    :comments-fn (lambda (color) (prism-blend color "black" 0.5))
    :strings-fn (lambda (color) (prism-blend color "black" 0.5))
    :colors (ef-themes-with-colors
              (list fg-main
                    magenta
                    cyan-cooler
                    magenta-cooler
                    blue
                    magenta-warmer
                    cyan-warmer
                    red-cooler
                    green
                    fg-main
                    cyan
                    yellow
                    blue-warmer
                    red-warmer
                    green-cooler
                    yellow-faint))))

(use-package prism
  :straight (prism :type git :host github :repo "alphapapa/prism.el")
  :config
  (require 'prism)
  (rko/setup-prism-for-light-theme))
  
(use-package git-gutter
  :straight (git-gutter :type git :host github :repo "emacsorphanage/git-gutter")
  :diminish t
  :init
  (custom-set-variables
   '(git-gutter:modified-sign "  ")
   '(git-gutter:added-sign "  ")
   '(git-gutter:deleted-sign "  ")
   '(git-gutter:update-interval 2))

  ;; (set-face-background 'git-gutter:modified "gold3")
  ;; (set-face-background 'git-gutter:added "green3")
  ;; (set-face-background 'git-gutter:deleted "red3")
  
  :config
  (global-git-gutter-mode +1))

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
