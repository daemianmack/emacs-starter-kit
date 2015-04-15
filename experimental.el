(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(global-set-key (kbd "C-c f") 'recentf-ido-find-file)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(defun clear-nrepl-server-buffer ()
  (interactive)
  (switch-to-buffer "*nrepl-server vium*")
  (erase-buffer)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun clear-all-nrepl-buffers ()
  (interactive)
  (clear-nrepl-server-buffer)
  (cider-find-and-clear-repl-buffer))

(defun load-buffer-clearing ()
  (interactive)
  (cider-load-buffer)
  (clear-all-nrepl-buffers))

(defun run-tests-fresh ()
  (interactive)
  (load-buffer-clearing)
  (with-current-buffer (cider-current-repl-buffer)
    (goto-char (point-max))
    (insert "(run-tests)")
    (cider-repl-return)))

(define-key cider-mode-map (kbd "C-c M-o") 'clear-all-nrepl-buffers)
(define-key cider-mode-map (kbd "C-c M-k") 'load-buffer-clearing)
(define-key cider-mode-map (kbd "C-c M-r") 'run-tests-fresh)

;; Expand this to all programming modes.
(add-hook 'clojure-mode-hook '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))

(setq helm-M-x-fuzzy-match t
      helm-apropos-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t
      helm-completion-in-region-fuzzy-match t
      helm-file-cache-fuzzy-match t
      helm-imenu-fuzzy-match t
      helm-locate-fuzzy-match t)

(require 'yasnippet)
(require 'clojure-snippets)
(yas-global-mode 1)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
;; Inherit local/ in a given mode by referencing it in that mode's .yas-parents.
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/local")
(yas-load-directory "~/.emacs.d/snippets")

(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'cider-mode))
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-start nil)
(setq ac-quick-help-delay 0.3)
(setq ac-trigger-key "M-/")

(setq desktop-restore-frames t)
(setq desktop-restore-in-current-display t)
(setq desktop-restore-forces-onscreen nil)
