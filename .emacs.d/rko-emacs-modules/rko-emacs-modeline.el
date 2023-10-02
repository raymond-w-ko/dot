;; -*- lexical-binding: t -*-

;; this is built-in to emacs 29
(defun rko/mode-line-window-selected-p ()
  "Return non-nil if we're updating the mode line for the selected window.
This function is meant to be called in `:eval' mode line
constructs to allow altering the look of the mode line depending
on whether the mode line belongs to the currently selected window
or not."
  (let ((window (selected-window)))
    (or (eq window (old-selected-window))
	      (and (minibuffer-window-active-p (minibuffer-window))
	           (with-selected-window (minibuffer-window)
	             (eq window (minibuffer-selected-window)))))))

(defun rko/last-index-of (needle haystack)
  "Find the last index of NEEDLE in HAYSTACK."
  (let ((start 0)
        (index -1))
    (while (string-match needle haystack start)
      (setq index (match-beginning 0))
      (setq start (1+ index)))
    index))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface rko/modeline-face-small
  '((t :inherit mode-line-inactive :height 0.75))
  "Face for small text in the modeline.")

(defface rko/modeline-face-error
  '((t :foreground "black" :background "red"))
  "Face for error text in the modeline.")

(defface rko/modeline-face-warning
  '((t :foreground "black" :background "yellow"))
  "Face for warning text in the modeline.")

(defface rko/modeline-face-black
  '((t :foreground "gray" :background "#888888"))
  "Face for noticeable text in the modeline.")

(defface rko/modeline-face-green
  '((t :foreground "black" :background "green"))
  "Face for noticeable text in the modeline.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq mode-line-format nil)
;; (force-mode-line-update)

(defvar-local rko/modeline-major-mode
    '(:eval (let* ((full-name (symbol-name major-mode))
                   (short-name (if (s-ends-with? "-mode" full-name)
                                   (substring full-name 0 -5)
                                 full-name)))
              (concat (nerd-icons-icon-for-mode major-mode) " " short-name " "))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Based on `flymake--mode-line-counter'.
(defun rko/modeline-flymake-counter (type)
  "Compute number of diagnostics in buffer with TYPE's severity.
TYPE is usually keyword `:error', `:warning' or `:note'."
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type)
               (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    (when (cl-plusp count)
      (number-to-string count))))

(defvar-local rko/modeline-flymake
    '(:eval
      (let ((error-count (rko/modeline-flymake-counter :error))
            (warning-count (rko/modeline-flymake-counter :warning))
            (note-count (rko/modeline-flymake-counter :note)))
        (when (or error-count warning-count note-count)
          (concat (when error-count
                    (propertize (concat " " error-count " ") 'face 'rko/modeline-face-error))
                  (when warning-count
                    (propertize (concat " " warning-count " ") 'face 'rko/modeline-face-warning))
                  (when note-count
                    (concat "📝" note-count " ")))))))

(defvar-local rko/modeline-flycheck
    '(:eval
      (let* ((total-counts (flycheck-count-errors flycheck-current-errors))
             (warning-pair (car total-counts))
             (error-pair (car (cdr total-counts)))
             (error-count (cdr error-pair))
             (warning-count (cdr warning-pair)))
        (when (or error-count warning-count)
          (concat (when error-count
                    (propertize (concat " " (prin1-to-string error-count) " ") 'face 'rko/modeline-face-error))
                  (when warning-count
                    (propertize (concat " " (prin1-to-string warning-count) " ") 'face 'rko/modeline-face-warning)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rko/modeline-just-last-path-segment (p)
  (let* ((p (string-replace "\\" "/" p))
         (p (if (s-ends-with? "/" p)
                (substring p 0 -1)
              p))
         (slash-index (or (rko/last-index-of "/" p) -1)))
    (substring p (1+ slash-index))))

(defun rko/modeline-project-buffer-name ()
  (when-let* ((proj (project-current))
              (root (project-root proj))
              (file (when buffer-file-name
                      (file-relative-name buffer-file-name root))))
      (when (and (stringp root) (stringp file))
        (concat "📁"
                (propertize (concat " " (rko/modeline-just-last-path-segment root) " ")
                            'face 'rko/modeline-face-small)
                (nerd-icons-icon-for-file file)
                " "
                file))))

(defun rko/modeline-tramp-buffer-name ()
  (when (and buffer-file-name
             (stringp buffer-file-name)
             (file-remote-p buffer-file-name))
    (let ((m (tramp-dissect-file-name buffer-file-name)))
      (when m
        (concat "🌐"
                (propertize (concat " "(tramp-file-name-host m) " ")
                            'face 'rko/modeline-face-small)
                (rko/modeline-project-buffer-name))))))

(defvar-local rko/modeline-buffer-name
    '(:eval (or (rko/modeline-tramp-buffer-name)
                (rko/modeline-project-buffer-name)
                (buffer-file-name)
                (buffer-name)
                " [no buffer name] ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local rko/modeline-readonly
    '(:eval (when buffer-read-only
              (propertize " 🔒 " 'face 'rko/modeline-face-black))))

(defvar-local rko/modeline-modified
    '(:eval (when (and (buffer-file-name) (buffer-modified-p))
              (propertize " 💾 " 'face 'rko/modeline-face-warning))))

(defvar-local rko/modeline-builtin-misc
    '(:eval (propertize (format-mode-line " L%l C%c %Z ")
                        'face 'mode-line-inactive)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dolist (x '(rko/modeline-major-mode
             rko/modeline-flymake
             rko/modeline-flycheck
             rko/modeline-buffer-name
             rko/modeline-readonly
             rko/modeline-modified
             rko/modeline-builtin-misc))
  (put x 'risky-local-variable t))

(setq-default
 mode-line-format
 '(" %e "
   rko/modeline-readonly
   rko/modeline-modified
   rko/modeline-major-mode
   rko/modeline-flymake
   rko/modeline-flycheck
   rko/modeline-buffer-name
   "    "
   rko/modeline-builtin-misc))

(provide 'rko-emacs-modeline)
