(setq dotfiles-dir (file-name-directory
		    (or (buffer-file-name) load-file-name)))


;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(beacon-mode 1)
(column-number-mode t)
(desktop-save-mode 1)
(global-subword-mode 1)
(global-undo-tree-mode)
(winner-mode 1)


(add-to-list 'auto-mode-alist '("\\.org" . org-mode))
(add-to-list 'auto-mode-alist '("\\.s\?css\\.*" . css-mode))
(add-to-list 'auto-mode-alist '("\\.js\\.*" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.coffee\\.*" . coffee-mode))

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'ruby-mode-hook 'flymake-mode)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
;; Run shellhist, which preserves command history in eshell within
;; applications (e.g., python console or mysql prompt).
(add-hook 'eshell-mode-hook 'shellhist-instrument-eshell)
;; Fix junk characters in shell mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; Let C-xk kill buffers as normal even when there's a client listening.
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
;; Does the above obsolete this?
(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (when server-buffer-clients
              (local-set-key (kbd "C-x C-k") 'server-edit))))

(defun m-eshell-hook ()
  ;; define control p, control n and the up/down arrow.
  (define-key eshell-mode-map [(control p)] 'eshell-previous-matching-input-from-input)
  (define-key eshell-mode-map [(control n)] 'eshell-next-matching-input-from-input)
  (define-key eshell-mode-map [up] 'eshell-previous-matching-input-from-input)
  (define-key eshell-mode-map [down] 'eshell-next-matching-input-from-input)
  (add-hook 'eshell-mode-hook 'm-eshell-hook))

(defun my-isearch-yank-word-hook ()
  (when (equal this-command 'my-isearch-word-at-point)
    (let ((string (concat "\\<"
                          (buffer-substring-no-properties
                           (progn (skip-syntax-backward "w_") (point))
                           (progn (skip-syntax-forward "w_") (point)))
                          "\\>")))
      (if (and isearch-case-fold-search
               (eq 'not-yanks search-upper-case))
          (setq string (downcase string)))
      (setq isearch-string string
            isearch-message
            (concat isearch-message
                    (mapconcat 'isearch-text-char-description
                               string ""))
            isearch-yank-flag t)
      (isearch-search-and-update))))
(add-hook 'isearch-mode-hook 'my-isearch-yank-word-hook)

; Makes region-killing work in graphical emacs.
(defun region-active-p ()  (and transient-mark-mode mark-active))
