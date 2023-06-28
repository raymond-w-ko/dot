;; -*- lexical-binding: t -*-

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
