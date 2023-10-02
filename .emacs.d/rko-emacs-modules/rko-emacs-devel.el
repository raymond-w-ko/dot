;; -*- lexical-binding: t -*-

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
  :bind (:map magit-status-mode-map
              ("C-M-i" . nil))
  :init
  nil
  :config
  (magit-auto-revert-mode -1))

(use-package diff-hl
  :straight t
  :config
  (global-diff-hl-mode -1))

(setq vterm-always-compile-module t)
(use-package vterm :straight t
  :init
  (setq vterm-shell shell-file-name)
  ;; hack to fix tramp heredoc issue that is preventing tmux from getting a tty
  (setq vterm-tramp-shells '(("docker" "sh")
                             ("ssh" "'zsh'"))))
(use-package multi-vterm :straight t)

(use-package rg
  :straight (rg :host github :repo "dajva/rg.el")
  :config
  (rg-enable-default-bindings))

(use-package flycheck
  :straight t
  :init
  nil
  :config
  (global-flycheck-mode +1))

(use-package flycheck-eglot
  :after (flycheck eglot)
  :straight (flycheck-eglot :host github :repo "intramurz/flycheck-eglot")
  :init
  nil
  :custom
  (flycheck-eglot-exclusive t)
  :config
  (global-flycheck-eglot-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(connection-local-set-profile-variables
 'remote-detached
 '((detached-shell-program . "/bin/bash")
   (detached-session-directory . "~/dtach-sessions")
   (detached-dtach-program . "dtach")))

(connection-local-set-profiles
 '(:application tramp :protocol "ssh") 'remote-detached)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'rko-emacs-devel)
