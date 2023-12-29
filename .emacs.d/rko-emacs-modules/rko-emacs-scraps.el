;;; rko-emacs-scraps --- -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(use-package idle-highlight
  :disabled
  :straight t
  :hook ((prog-mode text-mode) . idle-highlight-mode)
  :init
  (setq idle-highlight-idle-time 1.0))

(use-package telephone-line
  :disabled
  :straight t
  :init
  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
        telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
        telephone-line-primary-right-separator 'telephone-line-cubed-right
        telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-minor-mode-segment
                     telephone-line-buffer-segment))))
  (setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (evil   . (telephone-line-airline-position-segment))))
  (setq telephone-line-height 18
        telephone-line-evil-use-sort-tag t)
  :config
  (telephone-line-mode 1))

(use-package centaur-tabs
  :disabled
  :straight t
  :init
  (setq centaur-tabs-style "bar"
        centaur-tabs-height 22
        centaur-tabs-enable-key-bindings t
        centaur-tabs-set-bar 'under
        x-underline-at-descent-line t
        centaur-tabs--buffer-show-groups nil
        centaur-tabs-set-icons nil
        centaur-tabs-set-modified-marker t
        centaur-tabs-show-navigation-buttons nil
        centaur-tabs-enable-ido-completion nil
        centaur-tabs-adjust-buffer-order t)
  :config
  (centaur-tabs-mode t)
  (setq uniquify-separator "/")
  (setq uniquify-buffer-name-style 'forward)
  (centaur-tabs-headline-match)
  (centaur-tabs-change-fonts (face-attribute 'default :font) 90)
  (defun entaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.
Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \\"Emacs\\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
                          help-mode))
       "Help")
      ((memq major-mode '(org-mode
                          org-agenda-clockreport-mode
                          org-src-mode
                          org-agenda-mode
                          org-beamer-mode
                          org-indent-mode
                          org-bullets-mode
                          org-cdlatex-mode
                          org-agenda-log-mode
                          diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer)))))))

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
  :disabled
  :straight t
  :init
  (setq avy-style 'words)
  (setq avy-background t)
  :bind (("C-:" . avy-goto-char-2))
  :config
  (avy-setup-default))

(use-package avy-zap
  :disabled
  :straight t
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  :disabled
  :straight t
  :init
  (setq avy-style 'words)
  (setq avy-background t)
  :bind (("C-:" . avy-goto-char-2))
  :config
  (avy-setup-default))

(use-package avy-zap
  :disabled
  :straight t
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; project management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package persp-mode
  :disabled
  :straight t
  :config
  (persp-mode -1))

(use-package workgroups2
  :disabled
  :straight t
  :init
  (setq wg-use-default-session-file nil)
  :config
  (workgroups-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package symbol-overlay
  :disabled
  :init
  (global-set-key (kbd "M-i") 'symbol-overlay-put)
  (global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
  (global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
  (global-set-key (kbd "<f7>") 'symbol-overlay-mode)
  (global-set-key (kbd "<f8>") 'symbol-overlay-remove-all)
  :straight t)

(use-package doom-modeline
  :disabled
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'rko-emacs-scraps)
;;; rko-emacs-scraps.el ends here
