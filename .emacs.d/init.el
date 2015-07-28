(require 'cl)

(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")
        ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

(defvar packages-list
  '(evil
    evil-leader
    evil-tabs
    evil-paredit
    evil-surround
    smooth-scrolling
    paredit
    key-chord
    color-theme-solarized)
  "List of packages needs to be installed at launch")

(defun has-package-not-installed ()
  (loop for p in packages-list
        when (not (package-installed-p p)) do (return t)
        finally (return nil)))

(when (has-package-not-installed)
  (message "%s" "Get latest versions of all packages...")
  (package-refresh-contents)
  (message "%s" " done.")
  (dolist (p packages-list)
    (when (not (package-installed-p p))
      (package-install p))))

(evil-mode)
(setq key-chord-two-keys-delay 0.1)
(key-chord-define evil-insert-state-map "fj" 'evil-normal-state)
(key-chord-define evil-insert-state-map "jf" 'evil-normal-state)
(key-chord-mode 1)

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
