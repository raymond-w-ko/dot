;;; rko-emacs-ui --- -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

(use-package pulsar
  :straight t
  :init
  (setq pulsar-face 'pulsar-green)
  (setq pulsar-pulse nil)
  :config
  (require 'pulsar)
  (pulsar-global-mode 1)
  (add-hook 'next-error-hook #'pulsar-pulse-line)
  (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
  (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry))

(use-package nerd-icons
  :straight t
  :init
  (setq nerd-icons-scale-factor 1.00)
  :config
  (when (and (not (equal system-type 'windows-nt))
             (not (file-exists-p "~/.local/share/fonts/NFM.ttf")))
    (nerd-icons-install-fonts t)))
(use-package nerd-icons-dired
  :straight t
  :hook ((dired-mode) . nerd-icons-dired-mode))

;; minions is a a minor-mode menu for the mode line
(use-package minions
  :straight t)

(defvar rko/modes-with-ligatures '(prog-mode html-mode))
(use-package ligature
  :straight (ligature :type git :host github :repo "mickeynp/ligature.el")
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))

  (dolist (mode rko/modes-with-ligatures)
    (ligature-set-ligatures
     mode
     '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%")))

  ;; Enable all JetBrains Mono ligatures in programming modes
  ;; (ligature-set-ligatures
  ;;  'prog-mode
  ;;  '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
  ;;    "/>" "//" "/*" "*>" "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||"
  ;;    "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|"
  ;;    "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*"
  ;;    "<*>" "<->" "<!--" ":>" ":<" ":::" "::" ":?" ":?>" ":=" "::=" "=>>"
  ;;    "==>" "=/=" "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!=" ">]" ">:"
  ;;    ">>-" ">>=" ">=>" ">>>" ">-" ">=" "&&&" "&&" "|||>" "||>" "|>" "|]"
  ;;    "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||" ".." ".?" ".=" ".-" "..<"
  ;;    "..." "+++" "+>" "++" "[||]" "[<" "[|" "{|" "??" "?." "?=" "?:" "##"
  ;;    "###" "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" ";;" "_|_"
  ;;    "__" "~~" "~~>" "~>" "~-" "~@" "$>" "^=" "]#"))

  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(require 'rko-emacs-modeline)

(defvar rko/theme-based-prism-colors
  (list "#000000"
        "#477818"
        "#b63a28"
        "#4e43ba"
        "#ac286c"
        "#932ea9"))
(defvar rko/rainbow-600-prism-colors
 (list "#000000"
       "#b85143"
       "#8b6c27"
       "#348037"
       "#0077b2"
       "#6167c9"
       "#617388"
       "#717171"))
(defvar rko/rainbow-700-prism-colors
  (list "#000000"
        "#9c4438"
        "#765b21"
        "#2c6c2f"
        "#006596"
        "#5756a6"
        "#526172"
        "#5f5f5f"))
(defvar rko/rainbow-800-prism-colors
  (list "#000000"
        "#7d372d"
        "#60491b"
        "#235726"
        "#005179"
        "#494582"
        "#424d5b"
        "#4c4c4c"))

(defun rko/setup-prism-for-light-theme ()
  "Set up prism for light theme."
  (require 'prism)
  (prism-set-colors
    :num 8
    :desaturations '(0) ; do not change---may lower the contrast ratio
    :lightens '(0)      ; same
    :comments-fn (lambda (color) (prism-blend color "#ccc" 0.5))
    :strings-fn (lambda (color) (prism-blend color "#000" 0.33))
    :colors rko/rainbow-600-prism-colors))

(defun rko:setup-prism-for-solarized-theme ()
  "Set up prism for light theme."
  (interactive)
  (require 'prism)
  (require 'solarized-palettes)
  (let* ((m solarized-selenized-dark-color-palette-alist)
         (base-text-color (alist-get 'base0 m))
         (dark-text-color (alist-get 'base00 m))
         (colors (list (alist-get 'base0 m)
                       (alist-get 'red m)
                       (alist-get 'green m)
                       (alist-get 'cyan m)
                       (alist-get 'yellow m)
                       (alist-get 'blue m)
                       (alist-get 'orange m)
                       (alist-get 'magenta m))))
    (prism-set-colors
      :num 8
      :desaturations '(0) ; do not change---may lower the contrast ratio
      :lightens '(0)      ; same
      :comments-fn (lambda (color) (solarized-color-blend color base-text-color 0.7))
      :strings-fn (lambda (color) (solarized-color-blend color dark-text-color 0.5))
      :colors colors)))

(use-package prism
  :straight (prism :type git :host github :repo "alphapapa/prism.el")
  :hook ((emacs-lisp-mode clojure-mode clojurescript-mode clojurec-mode) . prism-mode)
  :config
  (require 'prism)
  ;; (rko/setup-prism-for-light-theme)
  (rko:setup-prism-for-solarized-theme)
  nil)

(use-package git-gutter
  :straight (git-gutter :type git :host github :repo "emacsorphanage/git-gutter")
  :diminish t
  :init
  (custom-set-variables
   '(git-gutter:modified-sign " ")
   '(git-gutter:added-sign " ")
   '(git-gutter:deleted-sign " ")
   '(git-gutter:update-interval 0))

  ;; (set-face-background 'git-gutter:modified "gold3")
  ;; (set-face-background 'git-gutter:added "green3")
  ;; (set-face-background 'git-gutter:deleted "red3")

  :config
  (add-to-list 'git-gutter:update-commands 'save-buffer)
  (global-git-gutter-mode -1))

(use-package symbol-overlay
  :straight (symbol-overlay :type git :host github :repo "wolray/symbol-overlay")
  :init
  nil
  :config
  (let ((map (make-sparse-keymap)))
    (setq symbol-overlay-map map)))

(provide 'rko-emacs-ui)
;;; rko-emacs-ui.el ends here
