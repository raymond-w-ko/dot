(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package '(devil))
  (unless (package-installed-p package)
    (package-install package)))

(require 'devil)
(setq devil-lighter " \U0001F608")
(setq devil-prompt "\U0001F608 %t")
(global-devil-mode)
(global-set-key (kbd "C-,") 'global-devil-mode)

(setq select-enable-clipboard t)
(electric-pair-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Term" :foundry "UKWN" :slant normal :weight regular :height 90 :width normal)))))

(setq line-spacing nil)
(setq inhibit-startup-screen t)
(setq initial-buffer-choice t)
