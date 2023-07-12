;; -*- lexical-binding: t -*-

(require 'f)

(defvar wsl-clipboard-path-in-linux
  (concat "/mnt/c/Users/" (getenv "LOGNAME") "/clipboard.txt"))
(defvar wsl-clipboard-copy-command
  "powershell.exe -command 'set-clipboard -value (cat $HOME/clipboard.txt -encoding utf8)' 2> /dev/null")
(defvar wsl-clipboard-paste-command
  "powershell.exe -command 'get-clipboard | out-file -encoding utf8 $HOME/clipboard.txt' 2> /dev/null")

(defun wsl-copy (start end)
  (interactive "r")
  (let ((x (buffer-substring start end)))
    (f-write-text x 'utf-8 wsl-clipboard-path-in-linux)
    (shell-command wsl-clipboard-copy-command)))

(defun wsl-paste ()
  (interactive)
  (shell-command wsl-clipboard-paste-command)
  (let ((clipboard (f-read-text wsl-clipboard-path-in-linux)))
    (insert clipboard)))

(provide 'rko-emacs-clipboard)
