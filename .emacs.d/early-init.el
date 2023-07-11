;; -*- lexical-binding: t -*-

(when (daemonp)
  (princ "don't run emacs as a daemon\n" 'external-debugging-output)
  (kill-emacs 1))

(defvar rko-emacs--file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1000 1000))
            file-name-handler-alist rko-emacs--file-name-handler-alist))

(setq read-process-output-max (* 1024 1024)
      package-enable-at-startup nil
      frame-resize-pixelwise t
      use-dialog-box t
      use-file-dialog nil
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-buffer-menu t
      initial-buffer-choice t
      line-spacing nil
      visible-bell t
      blink-cursor-mode -1
      scroll-margin 3)

(set-default-coding-systems 'utf-8)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; make emacs not store in a version controlled directory
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))
(make-directory user-emacs-directory t)
(when (boundp 'native-comp-eln-load-path)
  (setq native-comp-async-report-warnings-errors nil)
  (startup-redirect-eln-cache (expand-file-name "eln-cache" user-emacs-directory)))
(setq custom-file (expand-file-name "rko-custom.el" "~/.emacs.d"))

(setq rko-emacs-include-path-env-var
      (concat "C_INCLUDE_PATH=" (expand-file-name "~/emacs/include")))
