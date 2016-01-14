;; normalize directories in case symlinks are used
(defvar emacs-d
  (file-name-directory
   (file-chase-links load-file-name)))

(setq package-user-dir
      (expand-file-name "elpa" emacs-d))
(package-initialize)
(setq package-archives
      '(("org" . "http://orgmode.org/elpa/")
	("melpa" . "https://melpa.org/packages/")
	("gnu" . "https://elpa.gnu.org/packages/")))
(package-refresh-contents)

(defvar my-packages
  '(evil
    evil-leader
    evil-tabs
    evil-paredit
    evil-surround
    evil-escape
    smooth-scrolling
    paredit
    key-chord
    lispy
    avy
    ace-window
    projectile
    color-theme-solarized))
(dolist (package my-packages)
  (unless (package-installed-p package)
    (ignore-errors (package-install package))))
(defun my-upgrade-packages ()
  (save-window-excursion
    (package-list-packages t)
    (package-menu-mark-upgrades)
    (condition-case nil
	(package-menu-execute t)
      (error
       (package-menu-execute)))))

(add-hook 'emacs-lisp-mode-hook
	  (lambda () (lispy-mode 1)))

(global-set-key (kbd "M-p") 'ace-window)
(global-set-key (kbd "π") 'avy-goto-char)

(add-to-list 'load-path "~/.emacs.d/config")
(require 'basic-settings)

;; (setq key-chord-two-keys-delay 0.1)
;; (key-chord-mode 1)

(blink-cursor-mode 0)
(global-hl-line-mode 1)

;; (require 'uniquify)
;; (setq
;;  uniquify-buffer-name-style 'post-forward
;;  uniquify-separator ":")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(frame-background-mode (quote dark)))

(if (eq system-type 'windows-nt)
    (begin (set-face-attribute 'default nil :font "Consolas 8")
	   (set-frame-font "Consolas 8" nil t)))

(load-theme 'solarized t)
