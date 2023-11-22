(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.cache/emacs/var/bmkp/current-bookmark.el")
 '(connection-local-criteria-alist
   '(((:application tramp :protocol "adb")
      tramp-adb-connection-local-default-shell-profile tramp-adb-connection-local-default-ps-profile)
     ((:application rg :machine "oryxvac.local")
      rg-vars-oryxvac.local)
     ((:application tramp :protocol "ssh")
      remote-detached)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-adb-connection-local-default-ps-profile
      (tramp-process-attributes-ps-args)
      (tramp-process-attributes-ps-format
       (user . string)
       (pid . number)
       (ppid . number)
       (vsize . number)
       (rss . number)
       (wchan . string)
       (pc . string)
       (state . string)
       (args)))
     (tramp-adb-connection-local-default-shell-profile
      (shell-file-name . "/system/bin/sh")
      (shell-command-switch . "-c"))
     (rg-vars-oryxvac.local
      (rg-executable . "/bin/rg"))
     (remote-detached
      (detached-shell-program . "/bin/bash")
      (detached-session-directory . "~/dtach-sessions")
      (detached-dtach-program . "dtach"))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
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
       "poetry" "run" "jedi-language-server"))))
 '(zoom-ignored-buffer-names '(" *vundo tree*"))
 '(zoom-ignored-major-modes '(vundo-mode))
 '(zoom-size '(100 . 50)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:slant normal :weight normal :width normal :height 100 :family "Fira Code"))))
 '(fixed-pitch ((t :inherit default)))
 '(fixed-pitch-serif ((t :inherit default :family "Fira Code")))
 '(hop-face-double-char-1 ((t (:inherit ef-themes-mark-delete))))
 '(hop-face-double-char-2 ((t (:inherit ef-themes-mark-other))))
 '(hop-face-single-char ((t (:inherit ef-themes-mark-select))))
 '(italic ((t (:slant normal))))
 '(mono-complete-preview-face ((t :inherit font-lock-comment-face)))
 '(variable-pitch ((t (:slant normal :weight normal :width normal :height 90 :family "Fira Code")))))
