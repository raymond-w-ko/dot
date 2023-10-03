;;; rko-emacs-clipboard.el --- OS clipboard interop -*- lexical-binding: t -*-
;;; Commentary:
;;; This is a hack to get the OS clipboard working in WSL.

;;; Code:

(require 'f)

(defvar wsl-clipboard-path-in-linux
  (concat "/mnt/c/Users/" (getenv "LOGNAME") "/clipboard.txt"))
(defvar wsl-clipboard-copy-command
  "powershell.exe -command 'set-clipboard -value (cat $HOME/clipboard.txt -encoding utf8)' 2> /dev/null")
(defvar wsl-clipboard-paste-command
  "powershell.exe -command 'get-clipboard | out-file -encoding utf8 $HOME/clipboard.txt' 2> /dev/null")

(defun wsl-copy (start end)
  "Copy the region from START to END to the Windows clipboard."
  (interactive "r")
  (let ((x (buffer-substring start end)))
    (f-write-text x 'utf-8 wsl-clipboard-path-in-linux)
    (let ((default-directory "~"))
      (shell-command wsl-clipboard-copy-command))))

(defun wsl-paste ()
  "Paste the Windows clipboard to the current buffer."
  (interactive)
  (let ((default-directory "~"))
    (shell-command wsl-clipboard-paste-command))
  (let ((clipboard (f-read-text wsl-clipboard-path-in-linux 'utf-8-with-signature-dos)))
    ;; for some reason there is a trailing newline, which we don't want
    (insert (substring-no-properties clipboard nil -1))))

(provide 'rko-emacs-clipboard)
;;; rko-emacs-clipboard.el ends here
