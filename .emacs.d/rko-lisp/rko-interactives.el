;; -*- lexical-binding: t -*-

(require 'dash)

(defun rko/set-current-window-text-width (w)
  (let* ((win (selected-window))
         (delta (- w (window-width win))))
    (window-resize win delta t)))

(defun rko/tab-new ()
  (interactive)

  (let* ((num-tabs (->> (frame-parameter (selected-frame) 'tabs)
                        (length)))
         (num-wins (length (window-list))))
    (when (or (> num-tabs 1)
              (> num-wins 1))
      (tab-new)))

  (let* ((fw (frame-width))
         (target-width 102)
         (n (floor (/ fw target-width)))
         (rem (- fw (* n 100)
                 10)))
    (dotimes (_ n)
      (split-window-right))
    (rko/set-current-window-text-width rem)
    (dotimes (_ n)
      (windmove-right)
      (rko/set-current-window-text-width target-width))
    (dotimes (_ n)
      (split-window-below)
      (windmove-left))))
      
(keymap-global-set "C-x t 2" 'rko/tab-new)
(define-key xah-fly-command-map (kbd "C-S-n") 'rko/tab-new)

(defun rko/yapf ()
  "Run 'yapf' on the current region. Expects TRAMP and poetry"
  (interactive)
  (let* ((dir (project-root (project-current t)))
         (file (file-relative-name (buffer-file-name) dir)))
    (if (and dir
             file
             (s-ends-with? ".py" file t))
        (progn
          (let ((default-directory dir))
            (let* ((p (point))
                   (tmp-in-file (make-temp-name "yapf-"))
                   (tmp-in-path (concat dir tmp-in-file ".py"))
                   (tmp-out-buffer (generate-new-buffer " *rko/yapf*"))
                   (cmd (concat "poetry run yapf " tmp-in-file)))
              (message "rko/yapf:\n  target dir: %s\n  orig: %s\n  in file: %s\n  cmd: %s"
                       dir file tmp-in-path cmd)
              (write-region nil nil tmp-in-file nil nil nil nil)
              (let ((resize-mini-windows nil))
                (shell-command cmd tmp-out-buffer))
              (replace-buffer-contents tmp-out-buffer)
              (kill-buffer tmp-out-buffer)
              (delete-file tmp-in-file)
              (goto-char p))))
      (error "not in a project, or not a Python file"))))

(provide 'rko-interactives)
