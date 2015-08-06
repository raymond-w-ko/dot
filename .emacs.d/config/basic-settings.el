;; try to improve slow performance on Windows,
;; is this even necessary nowadays? stackoverflow mentioned Windows XP era
(setq w32-get-true-file-attributes nil)

;; remove all backup and autosave files
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(winner-mode 1)

(setq
 inhibit-startup-message t)

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq initial-scratch-message nil)

(provide 'basic-settings)
