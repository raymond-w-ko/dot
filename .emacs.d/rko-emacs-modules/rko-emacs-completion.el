;; -*- lexical-binding: t -*-

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

(use-package orderless
  :straight t
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

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
  (require 'dash)
  (require 'f)
  (let* ((corfu-path (--some (and (string-match-p ".+/corfu$" it) it)
                             load-path))
         (corfu-extension-path (concat corfu-path "/extensions")))
    (message corfu-extension-path)
    (cl-assert (f-directory-p corfu-extension-path) "missing corfu extensions directory")
    (add-to-list 'load-path corfu-extension-path))
  (require 'corfu-info)
  (global-corfu-mode)
  nil)

(use-package cape
  :straight t
  :init
  ;; (add-to-list 'completion-at-point-functions 'cape-dabbrev)
  ;; (setq completion-at-point-functions
  ;;       (delete 'cape-dabbrev completion-at-point-functions))
  nil)

  (use-package copilot
    :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
    :ensure t
    :hook ((prog-mode . copilot-mode)
           (git-commit-mode . copilot-mode))
    :bind (("C-c M-f" . copilot-complete)
           :map copilot-completion-map
           ("C-g" . 'copilot-clear-overlay)
           ("M-p" . 'copilot-previous-completion)
           ("M-n" . 'copilot-next-completion)
           ("<tab>" . 'copilot-accept-completion)
           ("M-f" . 'copilot-accept-completion-by-word)
           ("M-<return>" . 'copilot-accept-completion-by-line))
    :init
    (setq copilot-log-max 100000)
    (setq copilot-log-messages nil)
    :config
    (require 'copilot))

(provide 'rko-emacs-completion)
