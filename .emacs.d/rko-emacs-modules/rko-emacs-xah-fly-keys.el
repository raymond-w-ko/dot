;;; rko-emacs-xah-fly-keys.el --- xah-fly-keys for emacs
;;; Commentary:
;;; This needs it's own file as it's pretty complicated. The end goal is switch between QWERTY and
;;; CC1 in the same session. I have to research if this is possible.

;;; Code:
(require 'dired)
(require 'xah-fly-keys)
(require 'copilot)
(require 'undo-fu)
(require 'hop)
(require 'rko-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rko:xah-fly-scraps ()
  "Scraps."

  ;; this is not necessary if xah-fly-use-control-key is nil
  (define-key global-map (kbd "C-<tab>") nil)
  (define-key global-map (kbd "C-S-<tab>") nil)
  (define-key global-map (kbd "C-S-<iso-lefttab>") nil)
  (define-key xah-fly-command-map (kbd "C-TAB") nil)
  (define-key xah-fly-insert-map (kbd "C-TAB") nil)
  (define-key xah-fly-command-map (kbd "C-S-<iso-lefttab>") nil)
  (define-key xah-fly-insert-map (kbd "C-S-<iso-lefttab>") nil)
  (define-key xah-fly-command-map (kbd "C-S-TAB") nil)
  (define-key xah-fly-insert-map (kbd "C-S-TAB") nil)

  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rko:reset-xah-fly-keys ()
  "Reset xah-fly-keys by recreating internal keymaps and redefining all keys.

REQUIRES something that calls (xah-fly-define-keys)"
  (setq xah-fly-shared-map (make-sparse-keymap))
  (setq xah-fly-command-map (cons 'keymap xah-fly-shared-map))
  (setq xah-fly-insert-map (cons 'keymap xah-fly-shared-map)))

(defun rko:xah-fly-qwerty-pinky ()
  "Due to how I prefer my QWERTY keyboard, the semicolon spot is actually RET.
It needs to act as Enter some of times in some major modes for convenience."
  (interactive)
  (cond
   ((eq major-mode 'dired-mode)
    (call-interactively #'dired-find-file))
   (t
    (call-interactively #'xah-end-of-line-or-block))))

(defun rko:xah-fly-allow-copilot-p ()
  "Predicate to determine if copilot should be enabled."
  xah-fly-insert-state-p)

(defun rko:xah-fly-escape-key ()
  "Escape key for xah-fly-keys that allow you to deactivate mark and abort edit.
It allows the user to just spam the key to get out of any situation."
  (interactive)
  (when (region-active-p)
    (deactivate-mark))
  (when (active-minibuffer-window)
    (abort-recursive-edit)))

(defun rko:xah-fly-setup-common ()
  "Setup common keys for both QWERTY and CC1."

  ;; use copilot.el only in insert mode
  (add-to-list 'copilot-enable-predicates 'rko:xah-fly-allow-copilot-p)
  (add-to-list 'copilot-enable-display-predicates 'rko:xah-fly-allow-copilot-p)

  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z"))

  (global-set-key (kbd "C-s") #'save-buffer)
  (global-set-key (kbd "C-z") #'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") #'undo-fu-only-redo)

  (global-set-key (kbd "M-v") #'scroll-down)

  (xah-fly--define-keys
   xah-fly-command-map
   '(("<escape>" . rko:xah-fly-escape-key)
     ("C-S-n" . rko/tab-new)
     ("C-n" . next-line)))

  (xah-fly--define-keys
   xah-fly-command-map
   '(("f" . undo-fu-only-undo)
     ("w" . tab-next)
     ("_" . tab-previous)))

  (xah-fly--define-keys
   xah-fly-leader-key-map
   '(("u" . consult-buffer)

     ("z h" . magit)

     ("c ." . project-find-file)))

  nil)

(defun rko:maybe-accept-copilot-completion ()
  "Accept copilot completion if it's active."
  (interactive)
  (when (and (boundp 'copilot-mode) copilot-mode)
    (call-interactively #'copilot-accept-completion)))

(defun rko:xah-fly-setup-qwerty ()
  "Setup xah-fly-keys for QWERTY."

  (define-key xah-fly-command-map (kbd "M-i") 'windmove-up)
  (define-key xah-fly-command-map (kbd "M-k") 'windmove-down)
  (define-key xah-fly-command-map (kbd "M-j") 'windmove-left)
  (define-key xah-fly-command-map (kbd "M-l") 'windmove-right)

  (define-key xah-fly-insert-map (kbd "M-i") 'windmove-up)
  (define-key xah-fly-insert-map (kbd "M-k") 'windmove-down)
  (define-key xah-fly-insert-map (kbd "M-j") 'windmove-left)
  (define-key xah-fly-insert-map (kbd "M-l") 'windmove-right)

  (define-key xah-fly-command-map (kbd "C-M-i") 'buf-move-up)
  (define-key xah-fly-command-map (kbd "C-M-k") 'buf-move-down)
  (define-key xah-fly-command-map (kbd"C-M-j") 'buf-move-left)
  (define-key xah-fly-command-map (kbd"C-M-l") 'buf-move-right)

  (define-key global-map (kbd "C-SPC") 'rko:maybe-accept-copilot-completion)
  (define-key xah-fly-command-map (kbd "RET") 'rko:xah-fly-qwerty-pinky)

  (define-key xah-fly-command-map (kbd "\\") 'revert-buffer)
  (define-key xah-fly-command-map (kbd "C-t") 'hop-word)
  (define-key xah-fly-insert-map (kbd "C-t") 'hop-word)

  (define-key xah-fly-command-map (kbd "<f5>") 'tab-bar-switch-to-prev-tab)
  (define-key xah-fly-command-map (kbd "<f6>") 'tab-bar-switch-to-next-tab)
  (define-key xah-fly-insert-map (kbd "<f5>") 'tab-bar-switch-to-prev-tab)
  (define-key xah-fly-insert-map (kbd "<f6>") 'tab-bar-switch-to-next-tab)

  (define-key xah-fly-command-map (kbd "<f7>") 'project-switch-project)
  (define-key xah-fly-command-map (kbd "<f8>") 'project-find-file)

  nil)

(defun rko:xah-fly-setup-cc1 ()
  "Setup xah-fly-keys for CC1."

  (define-key xah-fly-command-map (kbd "C-t") 'hop-word)
  (define-key xah-fly-insert-map (kbd "C-t") 'hop-word)

  (define-key xah-fly-command-map (kbd "M-p") 'windmove-up)
  (define-key xah-fly-command-map (kbd "M-d") 'windmove-down)
  (define-key xah-fly-command-map (kbd "M-t") 'windmove-left)
  (define-key xah-fly-command-map (kbd "M-s") 'windmove-right)

  (define-key xah-fly-command-map (kbd "C-M-p") 'buf-move-up)
  (define-key xah-fly-command-map (kbd "C-M-d") 'buf-move-down)
  (define-key xah-fly-command-map (kbd"C-M-t") 'buf-move-left)
  (define-key xah-fly-command-map (kbd"C-M-s") 'buf-move-right)

  nil)

(defun rko:switch-to-xah-qwerty ()
  "Switch to QWERTY xah-fly-keys."
  (interactive)
  (rko:reset-xah-fly-keys)
  (xah-fly-define-keys)
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys 1)
  (rko:xah-fly-setup-common)
  (rko:xah-fly-setup-qwerty)
  (setq hop-jump-keys rko:qwerty-hop-keys)
  nil)

(defun rko:switch-to-xah-cc1 ()
  "Switch to CC1 xah-fly-keys."
  (interactive)
  (rko:reset-xah-fly-keys)
  (xah-fly-define-keys)
  (xah-fly-keys-set-layout "cc1")
  (xah-fly-keys 1)
  (rko:xah-fly-setup-common)
  (rko:xah-fly-setup-cc1)
  (setq hop-jump-keys rko:cc1-hop-keys)
  nil)


(provide 'rko-emacs-xah-fly-keys)
;;; rko-emacs-xah-fly-keys.el ends here
