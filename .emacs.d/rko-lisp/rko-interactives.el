;; -*- lexical-binding: t -*-

(defun rko/tab-new ()
  (interactive)
  
  (tab-new)
  
  (split-window-right)
  (split-window-right)
  
  (split-window-below)
  
  (windmove-right)
  (split-window-below)
  
  (windmove-right)
  (split-window-below)
  
  (balance-windows))
(keymap-global-set "C-x t 2" 'rko/tab-new)

(provide 'rko-interactives)
