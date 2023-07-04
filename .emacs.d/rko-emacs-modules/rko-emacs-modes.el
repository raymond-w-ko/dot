;; -*- lexical-binding: t -*-

(use-package yasnippet :straight t
  :config
  (yas-global-mode 1))

(use-package vlf
  :straight t
  :init
  (require 'vlf-setup))

(use-package markdown-mode :straight t)
(use-package yaml-mode :straight t)

(let* ((lsp-bridge-dir (concat (expand-file-name "~/src/") "lsp-bridge")))
  (cl-assert (f-directory-p lsp-bridge-dir) "could not find lsp-bridge directory")
  (add-to-list 'load-path lsp-bridge-dir)
  (let* ((ip-file (expand-file-name "lsp-bridge/remote_file/ip.txt" user-emacs-directory))
         (ip-file-dir (file-name-directory ip-file)))
    (make-directory ip-file-dir t)
    (unless (file-exists-p ip-file)
      (with-temp-buffer (write-file ip-file))))
  ;; (require 'lsp-bridge)
  ;; (global-lsp-bridge-mode)
  nil)

(use-package clojure-mode :straight t)
(use-package cider :straight t)

(provide 'rko-emacs-modes)
