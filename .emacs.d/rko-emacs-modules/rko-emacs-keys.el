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
  ;; (setq hop-jump-keys (concat "uoemk"
                              ;; "fhtns"
                              ;; "ir"
                              ;; "aljy"
                              ;; "cdvp"
                              ;; (reverse "zqxgwb")))
  (setq hop-jump-keys "asdghklqwertyuiopzxcvbnmfj")
  ;; :bind (("C-t" . hop-word))
  :init
  nil)

(use-package xah-fly-keys
  :straight (xah-fly-keys :host github :repo "xahlee/xah-fly-keys")
  :config
  (define-key xah-fly-command-map (kbd "M-i") 'windmove-up)
  (define-key xah-fly-command-map (kbd "M-k") 'windmove-down)
  (define-key xah-fly-command-map (kbd "M-j") 'windmove-left)
  (define-key xah-fly-command-map (kbd "M-l") 'windmove-right)

  (define-key xah-fly-insert-map (kbd "M-i") 'windmove-up)
  (define-key xah-fly-insert-map (kbd "M-k") 'windmove-down)
  (define-key xah-fly-insert-map (kbd "M-j") 'windmove-left)
  (define-key xah-fly-insert-map (kbd "M-l") 'windmove-right)

  (define-key global-map (kbd "C-<tab>") nil)
  (define-key global-map (kbd "C-S-<tab>") nil)
  (define-key global-map (kbd "C-S-<iso-lefttab>") nil)
  (define-key xah-fly-command-map (kbd "C-TAB") nil)
  (define-key xah-fly-insert-map (kbd "C-TAB") nil)
  (define-key xah-fly-command-map (kbd "C-S-TAB") nil)
  (define-key xah-fly-insert-map (kbd "C-S-TAB") nil)
  
  (define-key xah-fly-command-map (kbd "y") 'undo-fu-only-undo)
  (define-key xah-fly-command-map (kbd "RET") 'xah-end-of-line-or-block)
  (define-key xah-fly-command-map (kbd "SPC f") 'consult-buffer)
  (define-key xah-fly-command-map (kbd "SPC RET") 'save-buffer)
  (define-key xah-fly-command-map (kbd "SPC TAB RET") 'indent-sexp)
  (define-key xah-fly-command-map (kbd "C-t") 'hop-word)
  (define-key xah-fly-insert-map (kbd "C-t") 'hop-word)
  (define-key xah-fly-command-map (kbd "SPC / m") 'magit)
  
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys 1)

  (progn
    (defun xah-fly-keys-escape ()
      (interactive)
      (when (region-active-p)
        (deactivate-mark))
      (when (active-minibuffer-window)
        (abort-recursive-edit)))

    (define-key xah-fly-command-map (kbd "<escape>") 'xah-fly-keys-escape))

  (progn
    (defvar xah-fly-keys-fast-keyseq-timeout 200)

    (defun xah-fly-keys-tty-ESC-filter (map)
      (if (and (equal (this-single-command-keys) [?\e])
               (sit-for (/ xah-fly-keys-fast-keyseq-timeout 1000.0)))
          [escape] map))

    (defun xah-fly-keys-lookup-key (map key)
      (catch 'found
        (map-keymap (lambda (k b) (if (equal key k) (throw 'found b))) map)))

    (defun xah-fly-keys-catch-tty-ESC ()
      "Setup key mappings of current terminal to turn a tty's ESC into `escape'."
      (when (memq (terminal-live-p (frame-terminal)) '(t pc))
        (let ((esc-binding (xah-fly-keys-lookup-key input-decode-map ?\e)))
          (define-key input-decode-map
                      [?\e] `(menu-item "" ,esc-binding :filter xah-fly-keys-tty-ESC-filter)))))

    (xah-fly-keys-catch-tty-ESC)

    (define-key key-translation-map (kbd "ESC") (kbd "<escape>")))
  nil)

(provide 'rko-emacs-keys)
