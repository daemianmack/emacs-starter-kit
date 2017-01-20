(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(global-set-key (kbd "C-c f") 'recentf-ido-find-file)

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

(setq desktop-restore-frames t)
(setq desktop-restore-in-current-display t)
(setq desktop-restore-forces-onscreen nil)

(setq cljr-sort-comparator 'cljr--semantic-comparator)

(defun rkn-print-results-on-next-line (value)
  (end-of-line)
  (newline)
  (insert (format ";; => %s" value)))

(defun rkn-nrepl-eval-newline-comment-print-handler (buffer)
  (nrepl-make-response-handler buffer
                               (lambda (buffer value)
                                 (with-current-buffer buffer
                                   (rkn-print-results-on-next-line value)))
                               '()
                               (lambda (buffer value)
                                 (with-current-buffer buffer
                                   (rkn-print-results-on-next-line value)))
                               '()))

(defun rkn-nrepl-interactive-eval-print (form)
  "Evaluate the given FORM and print the value in the current
  buffer on the next line as a comment."
  (let ((buffer (current-buffer)))
    (nrepl-send-string form
                       (rkn-nrepl-eval-newline-comment-print-handler buffer)
                       nrepl-buffer-ns)))

(defun rkn-eval-expression-at-point-to-comment ()
  (interactive)
  (let ((form (cider-last-sexp)))
    (rkn-nrepl-interactive-eval-print form)))

(define-key cider-mode-map (kbd "C-c C-e") 'rkn-eval-expression-at-point-to-comment)

(global-set-key (kbd "M-y") 'yank-pop)
(global-set-key (kbd "C-c M-y") 'helm-show-kill-ring)

(defadvice yank (around yank-indent)
   "Indents after yanking."
   (let ((point-before (point)))
     ad-do-it
       (indent-region point-before (point))))
(ad-activate 'yank)

(defadvice javarun (around javarun)
  (if (get-buffer "*java-output*")
      (kill-buffer "*java-output*"))
  (if (get-buffer "*javac-output*")
      (kill-buffer "*javac-output*"))
    ad-do-it)

(ad-activate 'javarun)



(setq ibuffer-display-summary nil)

(setq dired-dwim-target t)

(setq max-list-eval-depth 20000)

(defun cider-figwheel-repl ()
  (interactive)
  (save-some-buffers)
  (with-current-buffer (cider-current-repl-buffer)
    (goto-char (point-max))
        (insert "(require 'figwheel-sidecar.repl-api)
             (figwheel-sidecar.repl-api/start-figwheel!) ; idempotent
             (figwheel-sidecar.repl-api/cljs-repl \"dev\")")
        (cider-repl-return)))

(global-set-key (kbd "C-c M-f") #'cider-figwheel-repl)

(setq window-combination-resize t)

(defun crux-rename-file-and-buffer ()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let* ((new-name (read-from-minibuffer "New name: " filename))
             (containing-dir (file-name-directory new-name)))
        (make-directory containing-dir t)
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defalias 'crux-rename-buffer-and-file #'crux-rename-file-and-buffer)

(defun crux-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename delete-by-moving-to-trash)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defalias 'crux-delete-buffer-and-file #'crux-delete-file-and-buffer)

;; Indent Clojure's `comment` form like a defun -- don't line up non-first-line args under first-line args.
(put-clojure-indent 'comment 'defun)

(define-key org-mode-map (kbd "<M-S-right>") 'org-shiftright)
(define-key org-mode-map (kbd "<M-S-left>")  'org-shiftleft)
(define-key org-mode-map (kbd "<M-S-up>")    'org-shiftup)
(define-key org-mode-map (kbd "<M-S-down>")  'org-shiftdown)

;; It appears necessary to explicitly insert these keybindings into the org-mode-map.
;; `global-set-key` doesn't suffice. (?)
(define-key org-mode-map (kbd "S-<up>")
                '(lambda () (interactive) (windmove-emacs-or-tmux "up" "tmux select-pane -U")))
(define-key org-mode-map (kbd "S-<down>")
                '(lambda () (interactive) (windmove-emacs-or-tmux "down" "tmux select-pane -D")))
(define-key org-mode-map (kbd "S-<right>")
                '(lambda () (interactive) (windmove-emacs-or-tmux "right" "tmux select-pane -R")))
(define-key org-mode-map (kbd "S-<left>")
                '(lambda () (interactive) (windmove-emacs-or-tmux "left" "tmux select-pane -L")))

;; Dupe this here for convenience until all config is single-filed.
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq variable-files-dir (concat dotfiles-dir "var/"))

(use-package smex
  :init (setq smex-save-file (concat variable-files-dir "smex-items"))
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c M-x" . execute-extended-command)))

;; Define themeable faces. smart-mode-line will inherit from these.
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(use-package smart-mode-line
  :config
  (setq sml/theme nil)
  (sml/setup)
  (setq sml/name-width '(20 . 80))
  (setq sml/outside-modified-char "‽")
  (setq sml/modified-char "!")
  (setq-default mode-line-front-space
                '(:eval (concat (propertize "%4l" 'face 'mode-line-position-face)
                                ","
                                (propertize "%1c" 'face
                                            (if (>= (current-column) 80)
                                                'mode-line-80col-face
                                              'mode-line-position-face)))))
  (setq sml/replacer-regexp-list
        '(("^~/\\.emacs\\.d/elpa/" ":ELPA:")
          ("^~/\\.emacs\\.d/" ":ED:")
          ("^/sudo:.*:" ":SU:")
          ("^~/Documents/" ":Doc:")
          ("^~/Dropbox/" ":DB:")
          ("^~/relevance-smart-tab-organizer/" ":RE:"))))

(use-package vhl)

(use-package swoop
  :bind (("C-c o"   . swoop)
         ("C-c O"   . swoop-multi)
         ("C-c M-o" . swoop-pcre-regexp)
         ("C-c C-o" . swoop-back-to-last-position)))

(defun ido-disable-line-truncation ()
  (set (make-local-variable 'truncate-lines) nil))

(use-package ido
  :init (setq ido-save-directory-list-file (concat variable-files-dir "ido.last"))
  :config
  (use-package ido-preview)
  (use-package ido-ubiquitous
    :ensure t
    :config (ido-ubiquitous-mode 1))
  (use-package flx-ido
    :ensure t
    :config
    (flx-ido-mode 1)
    (setq ido-enable-flex-matching t)
    (setq ido-use-faces nil))
  (use-package ido-vertical-mode
    :ensure t
    :config
    (ido-vertical-mode 1)
    (setq ido-vertical-show-count t)
    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))
  (ido-mode 1)
  (ido-everywhere 1)

  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
  (add-hook 'ido-setup-hook
            (lambda ()
              (define-key ido-completion-map (kbd "M-w") 'ido-preview-backward)
              (define-key ido-completion-map (kbd "M-e") 'ido-preview-forward)))

  (setq ido-confirm-unique-completion t)
  (setq ido-create-new-buffer 'always)
  (setq ido-enable-prefix nil)
  (setq ido-enter-matching-directory 'first)
  (setq ido-max-prospects 10)
  (setq ido-max-work-file-list 50)
  (setq ido-mode 'both)
  (setq ido-use-faces t)
  (setq ido-use-filename-at-point 'guess)
  (setq ido-use-virtual-buffers t))

;; Expand this to all programming modes.
(add-hook 'clojure-mode-hook '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))

(defun cider-repl-command (cmd)
  "Execute commands on the cider repl"
  (cider-switch-to-repl-buffer)
  (goto-char (point-max))
  (insert cmd)
  (cider-repl-return)
  (cider-switch-to-last-clojure-buffer))

(defun cider-repl-clear-buffer-from-orbit ()
  (interactive)
  (cider-switch-to-repl-buffer)
  (cider-repl-clear-buffer)
  (cider-switch-to-last-clojure-buffer))

(defun cider-repl-reset ()
  (interactive)
  (save-some-buffers)
  (cider-repl-command "(dev/reset)"))

(use-package cider
  :ensure t
  :pin melpa-stable
  :bind (("C-c M-o" . cider-repl-clear-buffer-from-orbit)
         ("C-c d"   . cider-repl-reset))
  :config
  (use-package cider-eval-sexp-fu :ensure t)
  (use-package clj-refactor       :ensure t
    :init
    (cljr-add-keybindings-with-prefix "C-c M-r")
    (setq cljr-favor-prefix-notation nil)
    (setq cljr-warn-on-eval nil)
    :config
    (use-package cljr-helm :ensure t)
    (setq cljr-auto-clean-ns nil)
    (setq cljr-auto-sort-ns nil))

  (add-hook 'cider-repl-mode-hook 'paredit-mode)

  (define-key cider-repl-mode-map (kbd "C-c M-o") 'cider-repl-clear-buffer)

  (setq cider-eval-spinner-type 'box-in-box)
  (setq cider-font-lock-dynamically '(var))
  (setq cider-overlays-use-font-lock t)
  (setq cider-popup-on-error t)
  (setq cider-prompt-save-file-on-load 'always-save)
  (setq cider-prompt-save-file-on-load nil)
  (setq cider-repl-history-file "~/.lein/cider-repl-history")
  (setq cider-repl-result-prefix ";; => ")
  (setq cider-repl-use-clojure-font-lock t)
  (setq cider-show-error-buffer nil)
  (setq cider-use-overlays nil)
  (setq nrepl-buffer-name-separator "/"))

(use-package inferior-lisp
  ;; (setq inferior-lisp-program "lein repl")
  ;; (add-hook 'clojure-mode-hook
  ;;           '(lambda ()
  ;;              (define-key clojure-mode-map
  ;;                "\C-c\C-k"
  ;;                '(lambda ()
  ;;                   (interactive)
  ;;                   (let ((current-point (point)))
  ;;                     (goto-char (point-min))
  ;;                     (let ((ns-idx (re-search-forward clojure-namespace-name-regex nil t)))
  ;;                       (when ns-idx
  ;;                         (goto-char ns-idx)
  ;;                         (let ((sym (symbol-at-point)))
  ;;                           (message (format "Loading %s ..." sym))
  ;;                           (lisp-eval-string (format "(require '%s :reload)" sym))
  ;;                           (lisp-eval-string (format "(in-ns '%s)" sym)))))
  ;;                     (goto-char current-point))))))

  ;; (add-hook 'inferior-lisp-mode-hook
  ;;           '(lambda ()
  ;;              (define-key inferior-lisp-mode-map
  ;;                "\C-cl"
  ;;                '(lambda ()
  ;;                   (interactive)
  ;;                   (erase-buffer)
  ;;                   (lisp-eval-string "")))))
)

(use-package adaptive-wrap :ensure t)

(use-package diminish :ensure t
  :config
  (diminish 'beacon-mode)
  (diminish 'clj-refactor-mode)
  (diminish 'eldoc-mode)
  (diminish 'highlight-symbol-mode)
  (diminish 'subword-mode)
  (diminish 'paredit-mode " )( ") ;; ¯\_(ツ)_/¯
  (diminish 'projectile-mode) ;; Supplanted by smart-mode-line.
  (diminish 'undo-tree-mode)
  (diminish 'yas-minor-mode))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  ;; Show full relative paths.
  (setq projectile-show-paths-function 'projectile-hashify-with-relative-paths)
  (setq projectile-cache-file (concat variable-files-dir "projectile.cache"))
  (setq projectile-known-projects-file (concat variable-files-dir "projectile-bookmarks.eld")))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (use-package ido-completing-read+ :ensure t)
  (use-package magithub :ensure t)
  (setq magit-completing-read-function 'magit-ido-completing-read)
  (setq magit-diff-arguments '("--stat" "--no-ext-diff"))
  (setq magit-diff-section-arguments '("--no-ext-diff"))
  (setq magit-diff-refine-hunk 'all)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-process-popup-time 10)
  (setq magit-revert-buffers 't)
  (setq magit-repository-directories '("~/src"))
  (setq magit-repository-directories-depth 2)
  (add-hook 'ido-setup-hook
            (lambda ()
              (define-key ido-completion-map
                (kbd "C-x g") 'ido-enter-magit-status)))
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
          (delete-other-windows))
  ;; restore previously hidden windows
  ;; TODO HACK
  (defadvice magit-quit-window (around magit-restore-screen activate)
    (let ((current-mode major-mode))
      ad-do-it
      ;; we only want to jump to register when the last seen buffer
      ;; was a magit-status buffer.
      (when (eq 'magit-status-mode current-mode)
                  (jump-to-register :magit-fullscreen)))))

(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'prog-mode-hook 'add-watchwords)

(use-package rainbow-delimiters :ensure t
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(defun lisp-mode-setup ()
  (eldoc-mode)
  (paredit-mode 1)
  (whitespace-mode))

(add-hook 'lisp-mode-hook 'lisp-mode-setup)
(add-hook 'emacs-lisp-mode-hook 'lisp-mode-setup)

(use-package clojure-mode
  :ensure t
  :bind (("C-x M-r" . cljr-helm))
  :config
  (add-hook 'clojure-mode-hook 'lisp-mode-setup)
  (add-hook 'cider-repl-mode-hook 'lisp-mode-setup))
