;; -*- lexical-binding: t -*-

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
  :after (copilot)
  :config

  (define-key xah-fly-insert-map (kbd "C-n") 'next-line)
  
  (define-key xah-fly-command-map (kbd "M-i") 'windmove-up)
  (define-key xah-fly-command-map (kbd "M-k") 'windmove-down)
  (define-key xah-fly-command-map (kbd "M-j") 'windmove-left)
  (define-key xah-fly-command-map (kbd "M-l") 'windmove-right)

  (define-key xah-fly-insert-map (kbd "M-i") 'windmove-up)
  (define-key xah-fly-insert-map (kbd "M-k") 'windmove-down)
  (define-key xah-fly-insert-map (kbd "M-j") 'windmove-left)
  (define-key xah-fly-insert-map (kbd "M-l") 'windmove-right)

  (define-key xah-fly-command-map (kbd "<f5>") 'tab-bar-switch-to-prev-tab)
  (define-key xah-fly-command-map (kbd "<f6>") 'tab-bar-switch-to-next-tab)
  (define-key xah-fly-insert-map (kbd "<f5>") 'tab-bar-switch-to-prev-tab)
  (define-key xah-fly-insert-map (kbd "<f6>") 'tab-bar-switch-to-next-tab)

  (define-key xah-fly-command-map (kbd "<f7>") 'project-switch-project)
  (define-key xah-fly-command-map (kbd "<f8>") 'project-find-file)

  (defun rko/maybe-accept-copilot-completion ()
    (interactive)
    (when (and (boundp 'copilot-mode) copilot-mode)
      (call-interactively #'copilot-accept-completion)))

  (define-key global-map (kbd "C-SPC") 'rko/maybe-accept-copilot-completion)
  (define-key global-map (kbd "C-<tab>") nil)
  (define-key global-map (kbd "C-S-<tab>") nil)
  (define-key global-map (kbd "C-S-<iso-lefttab>") nil)
  (define-key xah-fly-command-map (kbd "C-TAB") nil)
  (define-key xah-fly-insert-map (kbd "C-TAB") nil)
  (define-key xah-fly-command-map (kbd "C-S-<iso-lefttab>") nil)
  (define-key xah-fly-insert-map (kbd "C-S-<iso-lefttab>") nil)
  (define-key xah-fly-command-map (kbd "C-S-TAB") nil)
  (define-key xah-fly-insert-map (kbd "C-S-TAB") nil)
  

  (defun rko/xah-right-pinky ()
    (interactive)
    (cond
     ((eq major-mode 'dired-mode)
      (call-interactively #'dired-find-file))
     (t
      (call-interactively #'xah-end-of-line-or-block))))
  (define-key xah-fly-command-map (kbd "RET") 'rko/xah-right-pinky)

  (define-key xah-fly-command-map (kbd "\\") 'revert-buffer)
  (define-key xah-fly-command-map (kbd "y") 'undo-fu-only-undo)
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

  ;; should not be required since :after is used
  ;; (require 'copilot)
  (defun rko/xah-fly-allow-copilot-p () xah-fly-insert-state-p)
  (add-to-list 'copilot-enable-predicates 'rko/xah-fly-allow-copilot-p)
  (add-to-list 'copilot-enable-display-predicates 'rko/xah-fly-allow-copilot-p)

  (define-key xah-fly-command-map (kbd "C-S-n") 'rko/tab-new)

  nil)

(provide 'rko-emacs-keys)
