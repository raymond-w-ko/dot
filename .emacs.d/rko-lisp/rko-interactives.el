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

(provide 'rko-interactives)
