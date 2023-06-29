;; -*- lexical-binding: t -*-

(use-package projectile
  :straight t
  :init
  (setq projectile-project-search-path '("~/dot/.emacs.d/"
                                         "~/src/"
                                         "~/.cache/emacs/straight/repos/"))
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

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

(provide 'rko-emacs-project)
