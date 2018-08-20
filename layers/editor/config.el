
;; highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#f7f7f7")
(set-face-foreground 'highlight nil)

(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
      (progn
        (defconst width 90)
        (add-to-list 'default-frame-alist (cons 'top 0))
        (add-to-list
         'default-frame-alist
         (cons 'left
               (/
                (- (x-display-pixel-width) (* (frame-char-width) width)) 2)))
        (add-to-list 'default-frame-alist (cons 'width width))
        (add-to-list
         'default-frame-alist
         (cons 'height
               (/ (x-display-pixel-height) (frame-char-height)))))))
(set-frame-size-according-to-resolution)
