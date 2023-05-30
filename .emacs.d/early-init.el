(set-default-coding-systems 'utf-8)
(setq gc-cons-threshold (* 100 1000 1000))
(setq read-process-output-max (* 1024 1024))
(setq package-enable-at-startup nil)

;; make emacs not store in a version controlled directory
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))
(make-directory user-emacs-directory t)
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "eln-cache" user-emacs-directory)))
