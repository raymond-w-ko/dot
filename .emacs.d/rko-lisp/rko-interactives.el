;;; rko-interactives --- M-x functions -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(require 'dash)
(require 'project)
(require 's)

(defun rko/set-current-window-text-width (w)
  "Set the current window's text width to W."
  (let* ((win (selected-window))
         (delta (- w (window-width win))))
    (window-resize win delta t)))

(defun rko/tab-new (universal-arg)
  "Create a new tab, and split it into 3 windows.
With a prefix arg, split into more windows.

UNIVERSAL-ARG: the prefix arg."
  (interactive "p")

  (let* ((num-tabs (->> (frame-parameter (selected-frame) 'tabs)
                        (length)))
         (num-wins (length (window-list))))
    (when (or (> num-tabs 1)
              (> num-wins 1))
      (tab-new)))

  (let* ((n 3)
         (split-more (if (< 0 universal-arg)
                         (/ universal-arg 4)
                       0)))

    (when (not (= split-more 0))
      (setq n (+ n split-more)))
    (message "(rko/tab-new: %s)" n)
    (dotimes (_ (1- n))
      (split-window-right))
    (dotimes (i n)
      (split-window-below)
      (when (< i (1- n))
        (windmove-right))))

  (balance-windows))

(keymap-global-set "C-x t 2" 'rko/tab-new)

(defun rko/yapf ()
  "Run \\='yapf\\=' on the current region.  Expects TRAMP and poetry."
  (interactive)
  (let* ((dir (project-root (project-current t)))
         (file (file-relative-name (buffer-file-name) dir))
         (ext (concat "." (file-name-extension file))))
    (if (and dir
             file
             (s-ends-with? ".py" file t))
        (progn
          (let ((default-directory dir))
            (let* ((p (point))
                   (tmp-in-file (concat (make-temp-name "yapf-") ext))
                   (tmp-in-path (concat dir tmp-in-file ".py"))
                   (tmp-out-buffer (generate-new-buffer " *rko/yapf*"))
                   (cmd (concat "poetry run yapf " tmp-in-file)))
              (message "rko/yapf:\n  target dir: %s\n  orig: %s\n  in file: %s\n  cmd: %s"
                       dir file tmp-in-path cmd)
              (write-region nil nil tmp-in-file nil nil nil nil)
              (let ((resize-mini-windows nil)
                    (display-buffer-alist
                     '(("\\*Shell Command Output\\*" display-buffer-same-window))))
                (shell-command cmd tmp-out-buffer))
              (replace-buffer-contents tmp-out-buffer)
              (kill-buffer tmp-out-buffer)
              (delete-file tmp-in-file)
              (goto-char p)))
          (balance-windows))
      (error "not in a project, or not a Python file"))))

(defvar rko/clang-format-allowed-exts
  (let ((h (make-hash-table :test 'equal)))
    (mapc (lambda (x) (puthash x t h))
          '(".c" ".cpp" ".h" ".hpp" ".cc" ".hh" ".js" ".jsx"))
    h))
(defun rko/clang-format ()
  "Run \\='clang-format\\=' on the current region.  Expects TRAMP and poetry."
  (interactive)
  (let* ((dir (project-root (project-current t)))
         (file (file-relative-name (buffer-file-name) dir))
         (ext (concat "." (file-name-extension file))))
    (if (and dir file (gethash ext rko/clang-format-allowed-exts))
        (progn
          (let ((default-directory dir))
            (let* ((p (point))
                   (tmp-in-file (concat (make-temp-name "clang-format-") ext))
                   (tmp-in-path (concat dir tmp-in-file ext))
                   (tmp-out-buffer (generate-new-buffer " *rko/clang-format*"))
                   (cmd (concat "clang-format " tmp-in-file)))
              (message "rko/clang-format:\n  target dir: %s\n  orig: %s\n  in file: %s\n  cmd: %s"
                       dir file tmp-in-path cmd)
              (write-region nil nil tmp-in-file nil nil nil nil)
              (let ((resize-mini-windows nil)
                    (display-buffer-alist
                     '(("\\*Shell Command Output\\*" display-buffer-same-window))))
                (shell-command cmd tmp-out-buffer))
              (replace-buffer-contents tmp-out-buffer)
              (kill-buffer tmp-out-buffer)
              (delete-file tmp-in-file)
              (goto-char p))))
      (error "not in a project, or not a Python file: %s" ext))))

(defvar rko/prettier-allowed-exts
  (let ((h (make-hash-table :test 'equal)))
    (mapc (lambda (x) (puthash x t h))
          '(".js" ".jsx"))
    h))
(defun rko/prettier-format ()
  "Run \\='prettier\\=' on the current region.  Expects TRAMP and poetry."
  (interactive)
  (let* ((dir (project-root (project-current t)))
         (file (file-relative-name (buffer-file-name) dir))
         (ext (concat "." (file-name-extension file))))
    (if (and dir file (gethash ext rko/prettier-allowed-exts))
        (progn
          (let ((default-directory dir))
            (let* ((p (point))
                   (tmp-in-file (concat (make-temp-name "prettier-") ext))
                   (tmp-in-path (concat dir tmp-in-file))
                   (tmp-out-buffer (generate-new-buffer " *rko/prettier*"))
                   (tmp-error-buffer (generate-new-buffer " *rko/prettier-error*"))
                   (cmd (concat "~/npm-global/bin/prettier --no-color --log-level silent "
                                tmp-in-file)))
              (message "rko/prettier:\n  target dir: %s\n  orig: %s\n  in file: %s\n  cmd: %s"
                       dir file tmp-in-path cmd)
              (write-region nil nil tmp-in-file nil nil nil nil)
              (let ((resize-mini-windows nil)
                    (display-buffer-alist
                     '(("\\*Shell Command Output\\*" display-buffer-same-window))))
                (shell-command cmd tmp-out-buffer tmp-error-buffer))
              (replace-buffer-contents tmp-out-buffer)
              (kill-buffer tmp-out-buffer)
              (delete-file tmp-in-file)
              (goto-char p)
              (balance-windows))))
      (error "not in a project, or not a Python file: %s" ext))))

(provide 'rko-interactives)
;;; rko-interactives.el ends here
