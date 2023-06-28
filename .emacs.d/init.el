;; -*- lexical-binding: t -*-

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
        '((0 variable-pitch light 1.9)
          (1 variable-pitch light 1.8)
          (2 variable-pitch regular 1.7)
          (3 variable-pitch regular 1.6)
          (4 variable-pitch regular 1.5)
          (5 variable-pitch 1.4)      ; absence of weight means `bold'
          (6 variable-pitch 1.3)
          (7 variable-pitch 1.2)
          (t variable-pitch 1.1)))
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)
  (setq ef-themes-region '(intense no-extend neutral))
  (mapc #'disable-theme custom-enabled-themes)
  :config
  (ef-themes-select 'ef-elea-light))

(load custom-file t)

(use-package no-littering :straight t)

(require 'rko-emacs-builtin)

(eval
 `(use-package pcre
    :straight (pcre :host github :repo "syohex/emacs-pcre"
                    :pre-build ("make" ,rko-emacs-include-path-env-var "all")
                    :files (:defaults "pcre.el" "pcre-core.so"))))

(defun rko--test-pcre ()
  (require 'pcre)
  (let ((str "012-345-567"))
    (when (pcre-string-match "\\A(\\d+)-(\\d+)-(\\d+)\\z" str)
      (match-string 1 str)))

  (with-temp-buffer
    (insert "apple orange melon\n")
    (insert "red blue green\n")
    (insert "vim atom sublime\n")
    (goto-char (point-min))
    (let (matches)
      (while (pcre-re-search-forward "^\\S+ ([^[:space:]]+)" nil t)
        (push (match-string 1) matches))
      (reverse matches)))
  
  nil)

(pcre-string-match "[0-9]" "foo123bar")

(require 'rko-emacs-undo)
(require 'rko-emacs-keys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package diminish :straight t)
(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

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

(use-package persp-mode
  :disabled
  :straight t
  :config
  (persp-mode -1))

(use-package workgroups2
  :straight t
  :init
  (setq wg-use-default-session-file nil)
  :config
  (workgroups-mode +1))

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

(use-package bufferlo
  :straight (bufferlo :type git :host github :repo "florommel/bufferlo")
  :config
  (bufferlo-mode 1))

(defvar my-consult--source-buffer
  `(:name "Other Buffers"
          :narrow   ?b
          :category buffer
          :face     consult-buffer
          :history  buffer-name-history
          :state    ,#'consult--buffer-state
          :items ,(lambda () (consult--buffer-query
                              :predicate #'bufferlo-non-local-buffer-p
                              :sort 'visibility
                              :as #'buffer-name)))
  "Non-local buffer candidate source for `consult-buffer'.")

(defvar my-consult--source-local-buffer
  `(:name "Local Buffers"
          :narrow   ?l
          :category buffer
          :face     consult-buffer
          :history  buffer-name-history
          :state    ,#'consult--buffer-state
          :default  t
          :items ,(lambda () (consult--buffer-query
                              :predicate #'bufferlo-local-buffer-p
                              :sort 'visibility
                              :as #'buffer-name)))
  "Local buffer candidate source for `consult-buffer'.")

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
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark) ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop) ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)   ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
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
         ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history) ;; orig. next-matching-history-element
         ("M-r" . consult-history)) ;; orig. previous-matching-history-element
  :init
  (setq consult-buffer-sources '(consult--source-hidden-buffer
                                 my-consult--source-local-buffer
                                 my-consult--source-buffer))
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
  :straight (corfu :type git :host github :repo "minad/corfu"
                   :fork (:host nil :repo "git@github.com:raymond-w-ko/corfu"))
  :custom
  (corfu-count 8)
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
  (setq vterm-shell shell-file-name))
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

(use-package smart-mode-line
  :disabled
  :straight t
  :init
  (setq sml/theme 'respectful)
  :config
  (add-to-list 'sml/replacer-regexp-list '("^~/dot/\\.emacs\\.d/" ":ED:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/src/" ":SRC:"))
  :config
  (sml/setup))

(use-package nerd-icons
  :straight t
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
  (setq doom-modeline-project-detection 'projectile)
  (setq doom-modeline-minor-modes nil)
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

(defun rko/setup-post-frame-config (&optional frame)
  (use-package prism
    :straight (prism :type git :host github :repo "alphapapa/prism.el")
    :config
    (require 'prism)
    (rko/setup-prism-for-light-theme))
  
  (use-package dimmer
    :disabled
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
    (global-git-gutter-mode +1)))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'rko/setup-post-frame-config)
  (rko/setup-post-frame-config))
(setq rko/init t)
