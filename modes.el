(setq dotfiles-dir (file-name-directory
		    (or (buffer-file-name) load-file-name)))


;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(auto-fill-mode nil)
(column-number-mode t)
(desktop-save-mode 1)
(global-subword-mode 1)
(global-undo-tree-mode)
(highlight-symbol-mode)
(key-chord-mode 1)
(projectile-global-mode)
(smex-initialize)
(winner-mode 1)
(yas-global-mode 1)


(add-to-list 'auto-mode-alist '("\\.org" . org-mode))
(add-to-list 'auto-mode-alist '("\\.css\\.*" . rainbow-mode))
(add-to-list 'auto-mode-alist '("\\.js\\.*" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.clj" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.coffee\\.*" . coffee-mode))


(defun turn-on-paredit ()
  (paredit-mode 1)
  (define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-round))

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'ruby-mode-hook 'flymake-mode)
(add-hook 'clojure-mode-hook 'turn-on-paredit)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'clj-refactor-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
;; Make windmove work in org-mode.
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)
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

(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

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

;; full screen magit-status
(defadvice vc-annotate-status (around vc-annotate-fullscreen activate)
  (window-configuration-to-register :vc-annotate-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun vc-annotate-quit-session ()
  "Restores the previous window configuration and kills the vc-annotate buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :vc-annotate-fullscreen))

;; full screen vc-annotate
(defun vc-annotate-quit ()
  "Restores the previous window configuration and kills the vc-annotate buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :vc-annotate-fullscreen))

(eval-after-load "vc-annotate"
  '(progn
     (defadvice vc-annotate (around fullscreen activate)
       (window-configuration-to-register :vc-annotate-fullscreen)
       ad-do-it
       (delete-other-windows))

     (define-key vc-annotate-mode-map (kbd "q") 'vc-annotate-quit)))


(defadvice forward-sentence (after forward-sentence-advice activate) (recenter))
(defadvice backward-sentence (after backward-sentence-advice activate) (recenter))
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))


; Makes region-killing work in graphical emacs.
(defun region-active-p ()  (and transient-mark-mode mark-active))


(defalias 'inf-ruby-keys 'inf-ruby-setup-keybindings)


(add-to-list 'yas-snippet-dirs (concat dotfiles-dir "snippets"))
(yas-load-directory (concat dotfiles-dir "snippets"))
