(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(require 'cl)
(defvar packages-list
  '(evil
     evil-leader
     evil-tabs
     evil-paredit
     evil-surround
     smooth-scrolling
     paredit)
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

(require 'evil)
(evil-mode 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

(when (eq system-type 'gnu/linux)
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width semi-condensed :foundry "Misc" :family "Fixed"))))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)

