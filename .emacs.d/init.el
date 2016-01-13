;; normalize directories in case symlinks are used
(defvar emacs-d
  (file-name-directory
   (file-chase-links load-file-name)))
(setq package-user-dir
      (expand-file-name "elpa" emacs-d))

(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(setq package-enable-at-startup t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(evil
    evil-leader
    evil-tabs
    evil-paredit
    evil-surround
    evil-escape
    aggressive-indent
    smooth-scrolling
    paredit
    key-chord
    color-theme-solarized)
  "A list of packages that are ensured to be installed at launch")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/config")

(require 'basic-settings)

; (setq key-chord-two-keys-delay 0.1)
; (key-chord-mode 1)

(blink-cursor-mode 0)
(global-hl-line-mode 1)

(require 'uniquify)
(setq 
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(frame-background-mode (quote dark)))

(set-face-attribute 'default nil :font "Consolas 8")
(set-frame-font "Consolas 8" nil t)

(load-theme 'solarized t)
