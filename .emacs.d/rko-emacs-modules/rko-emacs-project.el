;; -*- lexical-binding: t -*-

(require 'bookmark+)

(use-package projectile
  :disabled
  :straight t
  :init
  (setq projectile-project-search-path '("~/dot/.emacs.d/"
                                         "~/src/"
                                         "~/.cache/emacs/straight/repos/"))
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


(provide 'rko-emacs-project)
