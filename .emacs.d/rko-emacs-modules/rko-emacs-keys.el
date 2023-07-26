;; -*- lexical-binding: t -*-

(use-package devil
  :disabled
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

(use-package paredit
  :straight t
  :hook
  (((emacs-lisp-mode clojure-mode clojurescript-mode clojurec-mode) . enable-paredit-mode))
  :init
  nil)

(use-package lispy
  :straight t
  :bind (:map lispy-mode-map
              ("." . nil)) 
  ;; :hook
  ;; (((emacs-lisp-mode clojure-mode clojurescript-mode clojurec-mode) . (lambda () (lispy-mode 1))))
  :init
  nil)

(use-package hop
  :straight (hop :host github :repo "Animeshz/hop.el")
  :init
  (setq hop-jump-keys (concat "uoemk"
                              "fhtns"
                              "ir"
                              "aljy"
                              "cdvp"
                              (reverse "zqxgwb")))
  :bind (("C-t" . hop-word)))

(use-package xah-fly-keys
  :straight (xah-fly-keys :host github :repo "xahlee/xah-fly-keys")
  :config
  (define-key xah-fly-command-map (kbd "RET") 'xah-end-of-line-or-block)
  (define-key xah-fly-command-map (kbd "SPC f") 'consult-buffer)
  (define-key xah-fly-command-map (kbd "SPC RET") 'save-buffer)
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys 1))

(provide 'rko-emacs-keys)
