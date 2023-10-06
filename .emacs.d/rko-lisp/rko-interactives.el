;; -*- lexical-binding: t -*-

(require 'dash)

(defun rko/set-current-window-text-width (w)
  (let* ((win (selected-window))
         (delta (- w (window-width win))))
    (window-resize win delta t)))

(defun rko/tab-new (universal-arg)
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
