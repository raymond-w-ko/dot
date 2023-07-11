;; -*- lexical-binding: t -*-

(require 'dash)

(defun rko/tab-new ()
  (interactive)

  (let* ((num-tabs (->> (frame-parameter (selected-frame) 'tabs)
                        (length)))
         (num-wins (length (window-list))))
    (when (or (> num-tabs 1)
              (> num-wins 1))
      (tab-new)))
  
  (split-window-right)
  (split-window-below)
  (windmove-right)
  
  ;; (split-window-right)
  ;; (split-window-below)
  ;; (windmove-right)
  
  (split-window-below)
  (windmove-left)
  
  (balance-windows))
(keymap-global-set "C-x t 2" 'rko/tab-new)

(provide 'rko-interactives)
