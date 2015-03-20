(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(global-set-key (kbd "C-c f") 'recentf-ido-find-file)

;(setq nrepl-hide-special-buffers t)
;(setq cider-show-error-buffer nil)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)


(global-aggressive-indent-mode)


(require 'comment-dwim-2)
(define-key cider-mode-map (kbd "M-;") 'comment-dwim-2)
(setq comment-dwim-2--inline-comment-behavior 'reindent-comment)

(global-set-key (kbd "C-c C-c") '(lambda () (interactive) (message "Here is a message.")))

