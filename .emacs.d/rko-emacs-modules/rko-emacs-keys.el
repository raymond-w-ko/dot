;; -*- lexical-binding: t -*-

(use-package devil
  :straight t
  :init
  (setq devil-lighter " \U0001F608")
  (setq devil-prompt "\U0001F608 %t")
  :config
  (global-devil-mode)
  (define-key devil-mode-map (kbd ".") #'devil)
  (add-to-list 'devil-special-keys `(". ." . ,(devil-key-executor ".")))
  (setq devil-translations '((", z" . "C-")
			                       (". z" . "M-")
			                       (", ," . ",")
			                       (". ." . ".")
			                       ("," . "C-")
			                       ("." . "M-")))
  nil)

(use-package god-mode
  :disabled
  :straight t
  :config
  ;; (god-mode)
  ;; (global-set-key (kbd "<escape>") #'god-mode-all)
  nil)

(use-package lispy
  :straight t
  :hook ((emacs-lisp-mode . (lambda () (lispy-mode 1)))))

;; ace window
(use-package ace-window
  :disabled
  :straight t
  :bind
  (("M-o" . ace-window))
  :init
  (setq aw-dispatch-always t)
  :config
  (ace-window-display-mode 1))

(use-package avy
  :straight t
  :init
  (setq avy-style 'words)
  (setq avy-background t)
  :bind (("C-:" . avy-goto-char-2))
  :config
  (avy-setup-default))

(use-package avy-zap
  :straight t
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

(use-package hop
  :straight (hop :host github :repo "Animeshz/hop.el")
  :init
  (setq hop-jump-keys "xqzbwgjinoartepvdchmsu")
  :bind (("C-:" . hop-word)))

(provide 'rko-emacs-keys)
