(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.cache/emacs/var/bmkp/current-bookmark.el")
 '(custom-safe-themes
   '("f4157511d5d4a31766a01ce6aeef7329a39afbfa61f6f6a96a29bb97dc9e00b1" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default))
 '(git-gutter:added-sign " ")
 '(git-gutter:deleted-sign " ")
 '(git-gutter:modified-sign " ")
 '(git-gutter:update-interval 0)
 '(safe-local-variable-values
   '((eglot-server-programs
      ((python-mode python-ts-mode)
       "poetry" "run" "pyright-langserver" "--stdio"))
     (eglot-server-programs
      ((python-mode python-ts-mode)
       "poetry" "run" "jedi-language-server")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:slant normal :weight medium :width normal :height 100 :family "FiraCode Nerd Font"))))
 '(fixed-pitch ((t :inherit default)))
 '(fixed-pitch-serif ((t :inherit default :family "FiraCode Nerd Font")))
 '(hop-face-dim-unmatched ((t (:inherit default :foreground "#555555"))))
 '(hop-face-double-char-1 ((t (:inherit dired-flagged))))
 '(hop-face-double-char-2 ((t (:inherit gnus-group-news-4-empty))))
 '(hop-face-single-char ((t (:inherit dired-flagge))))
 '(italic ((t (:slant normal))))
 '(bold ((t (:weight medium))))
 '(mono-complete-preview-face ((t :inherit font-lock-comment-face)))
 '(variable-pitch ((t (:slant normal :weight normal :width normal :height 90 :family "FiraCode Nerd Font Propo")))))
