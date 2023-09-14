;; -*- lexical-binding: t -*-

(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

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
  :disabled
  :init
  (global-set-key (kbd "M-i") 'symbol-overlay-put)
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

;; minions is a a minor-mode menu for the mode line
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
  ;; (doom-modeline-mode -1)
  nil)

(require 'rko-emacs-modeline)

(defun rko/setup-prism-for-dark-theme ()
  (prism-set-colors :num 16
    :save t
    :desaturations (cl-loop for i from 0 below 16
                            collect (* i 30))
    :lightens (cl-loop for i from 0 below 16
                       collect (* -1 i 10))
    :colors (list "dodgerblue" "medium sea green" "sandy brown"))
  nil)

(defvar rko/theme-based-prism-colors
  (list "#000000"
        "#477818"
        "#b63a28"
        "#4e43ba"
        "#ac286c"
        "#932ea9"))
(defvar rko/rainbow-600-prism-colors
 (list "#000000"
       "#b85143"
       "#8b6c27"
       "#348037"
       "#0077b2"
       "#6167c9"
       "#617388"
       "#717171"))
(defvar rko/rainbow-700-prism-colors
  (list "#000000"
        "#9c4438"
        "#765b21"
        "#2c6c2f"
        "#006596"
        "#5756a6"
        "#526172"
        "#5f5f5f"))
(defvar rko/rainbow-800-prism-colors
  (list "#000000"
        "#7d372d"
        "#60491b"
        "#235726"
        "#005179"
        "#494582"
        "#424d5b"
        "#4c4c4c"))
(defun rko/setup-prism-for-light-theme ()
  (require 'prism)
  (prism-set-colors
    :num 8
    :desaturations '(0) ; do not change---may lower the contrast ratio
    :lightens '(0)      ; same
    :comments-fn (lambda (color) (prism-blend color "#ccc" 0.5))
    :strings-fn (lambda (color) (prism-blend color "#000" 0.33))
    :colors rko/rainbow-600-prism-colors))

(use-package prism
  :straight (prism :type git :host github :repo "alphapapa/prism.el")
  :hook ((emacs-lisp-mode clojure-mode clojurescript-mode clojurec-mode) . prism-mode)
  :config
  (require 'prism)
  (rko/setup-prism-for-light-theme))

(use-package git-gutter
  :straight (git-gutter :type git :host github :repo "emacsorphanage/git-gutter")
  :diminish t
  :init
  (custom-set-variables
   '(git-gutter:modified-sign " ")
   '(git-gutter:added-sign " ")
   '(git-gutter:deleted-sign " ")
   '(git-gutter:update-interval 0))

  ;; (set-face-background 'git-gutter:modified "gold3")
  ;; (set-face-background 'git-gutter:added "green3")
  ;; (set-face-background 'git-gutter:deleted "red3")
  
  :config
  (add-to-list 'git-gutter:update-commands 'save-buffer)
  (global-git-gutter-mode -1))

(provide 'rko-emacs-ui)
