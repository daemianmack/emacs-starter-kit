;; Dupe this here for convenience until all config is single-filed.
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq variable-files-dir (concat dotfiles-dir "var/"))

(use-package validate :ensure t)

;; Always load newest byte code.
(validate-setq load-prefer-newer t)

(validate-setq require-final-newline nil)

(defun delete-register (name)
  (setq register-alist
        (delq (assoc name register-alist)
              register-alist)))

;; Speed up display of very long lines.
(setq-default bidi-display-reordering nil)

(global-set-key (kbd "<f12>") 'bury-buffer)
(global-set-key (kbd "C-c C-i") 'bury-buffer)

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file:" recentf-list nil t)))
    (when file
      (find-file file))))

(use-package recentf :ensure t
  :init
  (recentf-mode 1)
  :bind
  (("C-c f f" . recentf-ido-find-file))
  :config
  (validate-setq recentf-max-saved-items 100)
  (validate-setq recentf-save-file (concat variable-files-dir ".recentf")))

(defun locate-org-files (search-string)
  "Adjust `locate-with-filter' to only search `org-mode' files with SEARCH-STRING."
  (interactive "sSearch string: ")
  (locate-with-filter search-string ".org$"))

(global-set-key (kbd "C-c i") 'locate-org-files)

;; TODO ctrl-up, ctrl-down allow in-place preview via
;; `helm-follow-action-forward` etc but only from within
;; `helm-buffers-list` -- get this working from within
;; `helm-find-files` too.
(use-package helm :ensure t
  :init
  (helm-mode 1)
  :bind
  ("C-x C-b" . helm-buffers-list)
  :config
  (validate-setq helm-mode-fuzzy-match t
                 helm-completion-in-region-fuzzy-match t
                 helm-follow-mode-persistent t)
  ;; Use Mac OS X's Spotlight.
  (validate-setq helm-locate-command "mdfind -name %s %s"))

(require 'yasnippet)
(require 'clojure-snippets)
(yas-global-mode 1)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
;; Inherit local/ in a given mode by referencing it in that mode's .yas-parents.
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/local")
(yas-load-directory "~/.emacs.d/snippets")

(validate-setq desktop-restore-frames t)
(validate-setq desktop-restore-in-current-display t)
(validate-setq desktop-restore-forces-onscreen nil)

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


(validate-setq dired-dwim-target t)

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

(validate-setq window-combination-resize t)

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

(use-package smex :ensure t
  :config (setq smex-save-file (concat variable-files-dir "smex-items"))
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

(defvar my-mode-line-buffer-line-count nil)
(make-variable-buffer-local 'my-mode-line-buffer-line-count)

(defun my-mode-line-count-lines ()
  (validate-setq my-mode-line-buffer-line-count
                 (int-to-string (count-lines (point-min) (point-max)))))

(add-hook 'find-file-hook 'my-mode-line-count-lines)
(add-hook 'after-save-hook 'my-mode-line-count-lines)
(add-hook 'after-revert-hook 'my-mode-line-count-lines)
(add-hook 'dired-after-readin-hook 'my-mode-line-count-lines)

(use-package smart-mode-line :ensure t
  :config
  (validate-setq sml/theme nil)
  (sml/setup)
  (validate-setq sml/name-width '(20 . 80))
  (validate-setq sml/outside-modified-char "‽")
  (validate-setq sml/modified-char "!")
  (setq-default mode-line-front-space
                '(:eval (concat (let ((str "%4l"))
                                  (when (and (not (buffer-modified-p)) my-mode-line-buffer-line-count)
                                    (setq str (concat str "/" my-mode-line-buffer-line-count)))
                                  str)
                                ":"
                                (propertize "%1c" 'face
                                            (if (>= (current-column) 80)
                                                'mode-line-80col-face
                                              'mode-line-position-face))
                                " "
                                (propertize "%p" 'face 'mode-line-position-face)
                                " ")))
  (validate-setq sml/replacer-regexp-list
        '(("^~/\\.emacs\\.d/elpa/" ":ELPA:")
          ("^~/\\.emacs\\.d/" ":ED:")
          ("^/sudo:.*:" ":SU:")
          ("^~/Documents/" ":Doc:")
          ("^~/Dropbox/" ":DB:")
          ("^~/relevance-smart-tab-organizer/" ":RE:"))))

(use-package vhl)

(use-package swoop :ensure t
  :bind (("C-c o"   . swoop)
         ("C-c O"   . swoop-multi)
         ("C-c M-o" . swoop-pcre-regexp)
         ("C-c C-o" . swoop-back-to-last-position))
  :config
  (bind-keys
   :map swoop-map
   ("<up>"   . swoop-action-goto-line-prev)
   ("<down>" . swoop-action-goto-line-next)
   ("C-p"    . prev-history-element)
   ("C-n"    . next-history-element)))

(defun ido-disable-line-truncation ()
  (set (make-local-variable 'truncate-lines) nil))

(use-package ido :ensure t
  :config
  (use-package ido-preview)
  (use-package ido-ubiquitous
    :ensure t
    :config (ido-ubiquitous-mode 1))
  (use-package flx-ido
    :ensure t
    :config
    (flx-ido-mode 1)
    (validate-setq ido-enable-flex-matching t)
    (validate-setq ido-use-faces nil))
  (use-package ido-vertical-mode
    :ensure t
    :config
    (ido-vertical-mode 1)
    (validate-setq ido-vertical-show-count t)
    (validate-setq ido-vertical-define-keys 'C-n-C-p-up-and-down))
  (validate-setq ido-save-directory-list-file (concat variable-files-dir "ido.last"))
  (ido-mode 1)
  (ido-everywhere 1)

  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
  (add-hook 'ido-setup-hook
            (lambda ()
              (define-key ido-completion-map (kbd "M-w") 'ido-preview-backward)
              (define-key ido-completion-map (kbd "M-e") 'ido-preview-forward)
              ;; "enter", "right", "TAB" all enter directory at point.
              (define-key ido-completion-map (kbd "<right>") 'ido-exit-minibuffer)
              (define-key ido-completion-map (kbd "TAB")     'ido-exit-minibuffer)
              ;; "left" walks back up one directory.
              (define-key ido-completion-map (kbd "<left>") 'ido-delete-backward-updir)))

  (validate-setq ido-confirm-unique-completion t)
  (validate-setq ido-create-new-buffer 'always)
  (validate-setq ido-enable-prefix nil)
  (validate-setq ido-enter-matching-directory 'first)
  (validate-setq ido-max-prospects 10)
  (validate-setq ido-max-work-file-list 50)
  (validate-setq ido-mode 'both)
  (validate-setq ido-use-faces t)
  (validate-setq ido-use-filename-at-point 'guess)
  (validate-setq ido-use-virtual-buffers t))

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


(defun my-cider-repl-prompt (namespace)
  "Return a prompt string that mentions NAMESPACE."
  (format "\n%s> " namespace))

(use-package cider
  :ensure t
  :pin melpa-stable
  :bind (("C-c M-o" . cider-repl-clear-buffer-from-orbit)
         ("C-c d"   . cider-repl-reset))
  :config
  (use-package cider-eval-sexp-fu :ensure t
    :config
    (validate-setq eval-sexp-fu-flash-duration 0.05))
  (use-package clj-refactor       :ensure t
    :init
    (cljr-add-keybindings-with-prefix "C-c M-r")
    (validate-setq cljr-favor-prefix-notation nil)
    (validate-setq cljr-warn-on-eval nil)
    :config
    (use-package cljr-helm :ensure t)
    (validate-setq cljr-auto-clean-ns nil)
    (validate-setq cljr-auto-sort-ns nil))

  (add-hook 'cider-repl-mode-hook 'paredit-mode)

  (define-key cider-repl-mode-map (kbd "C-c M-o") 'cider-repl-clear-buffer)

  (validate-setq cider-eval-spinner-type 'box-in-box)
  (validate-setq cider-font-lock-dynamically '(var))
  (validate-setq cider-overlays-use-font-lock t)
  (validate-setq cider-prompt-save-file-on-load 'always-save)
  (validate-setq cider-prompt-save-file-on-load nil)
  (validate-setq cider-repl-history-file "~/.lein/cider-repl-history")
  (validate-setq cider-repl-result-prefix ";; => ")
  (validate-setq cider-repl-use-clojure-font-lock t)
  (validate-setq cider-show-error-buffer nil)
  (validate-setq cider-use-overlays nil)
  (validate-setq nrepl-buffer-name-separator "/")
  (validate-setq cider-repl-prompt-function 'my-cider-repl-prompt)
  (validate-setq cider-pprint-fn 'fipp)
  (validate-setq cider-repl-history-separator "────────────────────────────────────────────────────────"))

(use-package inferior-lisp
  ;; (validate-setq inferior-lisp-program "lein repl")
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
  (validate-setq projectile-cache-file (concat variable-files-dir "projectile.cache"))
  (validate-setq projectile-known-projects-file (concat variable-files-dir "projectile-bookmarks.eld"))
  (add-to-list 'projectile-globally-ignored-directories "resources/public/js")
  (add-to-list 'projectile-globally-ignored-file-suffixes ".log")
  (add-to-list 'projectile-globally-ignored-directories "js/compiled"))

(use-package magit
  :ensure t
  :pin melpa-stable
  :bind (("C-x g" . magit-status)
         ("C-c f g" . magit-log-buffer-file))
  :config
  ;; Avoid version skew that breaks Magit's git-rebase-mode.
  (use-package with-editor :ensure t :pin melpa-stable)
  (use-package ido-completing-read+ :ensure t)
  (validate-setq magit-completing-read-function 'magit-ido-completing-read)
  (validate-setq magit-diff-arguments '("--stat" "--no-ext-diff"))
  (validate-setq magit-diff-section-arguments '("--no-ext-diff"))
  (validate-setq magit-diff-refine-hunk 'all)
  (validate-setq magit-fetch-arguments '("--prune"))
  (validate-setq magit-process-popup-time 10)
  (validate-setq magit-revert-buffers 't)
  (validate-setq magit-repository-directories '("~/src"))
  (validate-setq magit-repository-directories-depth 2)
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
        (progn (jump-to-register :magit-fullscreen)
               (delete-register :magit-fullscreen))))))

(defun add-watchwords ()
  (interactive)
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

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
  (add-to-list 'auto-mode-alist '("\\.clj" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljs" . clojurescript-mode))
  (add-to-list 'auto-mode-alist '("\\.edn" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.boot" . clojure-mode))
  (add-hook 'clojure-mode-hook 'lisp-mode-setup)
  (add-hook 'clojure-mode-hook 'turn-on-paredit)
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  (add-hook 'cider-repl-mode-hook 'lisp-mode-setup)
  ;; Indent Clojure's `comment` form like a defun -- don't line up non-first-line args under first-line args.
  (put-clojure-indent 'comment 'defun)
  ;; Font-lock Datomic's logic vars (`?user-id`).
  (font-lock-add-keywords 'clojure-mode
                          '(("?[:alnum:][[:alnum:][:punct:]]+" . font-lock-constant-face)))
  ;; This causes CLJS buffer errors to return REPL control more quickly.
  (validate-setq max-lisp-eval-depth 20000))

(use-package whitespace
  :ensure t
  :config
  (validate-setq whitespace-style '(face trailing tabs)))

(use-package uniquify
  :config
  (validate-setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package ivy
  :ensure t
  :config
  (validate-setq ivy-use-virtual-buffers t)
  )

(use-package swiper
  :ensure t
  :config
  (ivy-mode 1)
  (validate-setq ivy-extra-directories nil)
  (validate-setq enable-recursive-minibuffers t)
  (validate-setq ivy-virtual-abbreviate 'full)
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (bind-keys
   :map swiper-map
   ("C-r" . ivy-previous-line)))


(use-package dired+
  :ensure t)

(defun inf-clojurize-buffer ()
  ;; For now sometimes this is required in a buffer needing a connection to an inf REPL...
  ;; TODO Make this not necessary.
  (interactive)
  (make-local-variable 'inf-clojure-buffer)
  (validate-setq inf-clojure-buffer "*inf-clj*"))

(defun inf-clojure-repl-edit-last-sexp ()
  "Send the previous sexp to the inferior Clojure process for editing."
  (interactive)
  (let ((str (buffer-substring-no-properties
              (save-excursion (backward-sexp) (point))
              (point))))
    (with-current-buffer inf-clojure-buffer
      (insert str))
    (inf-clojure-switch-to-repl t)))

(use-package inf-clojure
  :ensure t
  :bind (("C-c M-p" . inf-clojure-repl-edit-last-sexp)  ;; Mimic + clobber CIDER's.
         ("C-c M-o" . inf-clojure-clear-repl-buffer))
  :config
  (validate-setq inf-clojure-program "nc localhost 5554")
  (add-hook 'inf-clojure-mode-hook #'lisp-mode-setup)
  (add-hook 'clojure-mode-hook 'clojure-custom-setup)
  (validate-setq inf-clojure-generic-cmd '("localhost" . 5554))
  ;; For some reason paredit is missing this in inf-clojure REPLs.
  (add-hook 'paredit-mode-hook
            (lambda ()
              (when (>= paredit-version 21)
                (define-key inf-clojure-mode-map "{" 'paredit-open-curly)
                (define-key inf-clojure-mode-map "}" 'paredit-close-curly)))))

(use-package pinentry
  :ensure t
  :config
  (setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))
  (pinentry-start))

(use-package yaml-mode
  :ensure t)

(use-package back-button
  :ensure t
  :config
  (use-package visible-mark :ensure t)
  (back-button-mode 1))


;; Change org keybindings from the default of...
;;   shift-arrow      ;; cycle TODO states
;;   ctrl-shift-arrow ;; clock stuff
;; to
;;   shift-arrow ;; preserve windmove movement
;;   ctrl-arrow  ;; cycle TODO states
;; Additional terminal config required to support default org keybindings...
;;   ctrl-shift-up    [1;6A
;;   ctrl-shift-down  [1;6B
;;   ctrl-shift-right [1;6C
;;   ctrl-shift-left  [1;6D
(use-package org-mode
  :init
  (unbind-key "S-<up>" org-mode-map)
  (unbind-key "S-<down>" org-mode-map)
  (unbind-key "S-<right>" org-mode-map)
  (unbind-key "S-<left>" org-mode-map)
  (validate-setq org-src-fontify-natively t)
  (validate-setq org-hide-leading-stars t)
  (validate-setq org-return-follows-link t)
  (validate-setq org-cycle-separator-lines -1)
  (validate-setq org-yank-folded-subtrees nil)
  (validate-setq org-yank-adjusted-subtrees t)
  (validate-setq org-confirm-babel-evaluate nil)
  (require 'ob-clojure)
;  (use-package org-babel-clojure :ensure t)
  (require 'cider)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)
     (clojure . t)))
  :bind
  (:map org-mode-map
        ("<C-right>" . org-shiftright) ;; Ctrl+<arrow> to cycle TODO states.
        ("<C-left>"  . org-shiftleft)
        ("<C-up>"    . org-shiftup)
        ("<C-down>"  . org-shiftdown))
  :config
  (use-package org-cliplink :ensure t)
  (validate-setq org-babel-clojure-backend 'cider)
  (validate-setq org-babel-ditaa-java-cmd "java -Dapple.awt.UIElement=true")
  (validate-setq org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.9/libexec/ditaa0_9.jar")
  ;; Show all empty lines when collapsed.
  (make-face 'org-inflight-face)
  (validate-setq org-todo-keyword-faces '(("DOING" . org-inflight-face)))
  )

;; Integrate with tmux splits.
(global-set-key (kbd "S-<up>")
                '(lambda () (interactive) (windmove-emacs-or-tmux "up"  "tmux select-pane -U")))
(global-set-key (kbd "S-<down>")
                '(lambda () (interactive) (windmove-emacs-or-tmux "down"  "tmux select-pane -D")))
(global-set-key (kbd "S-<right>")
                '(lambda () (interactive) (windmove-emacs-or-tmux "right" "tmux select-pane -R")))
(global-set-key (kbd "S-<left>")
                '(lambda () (interactive) (windmove-emacs-or-tmux "left"  "tmux select-pane -L")))


(defun wwai-repl ()
  (interactive)
  (inf-clojure "bin/run script/repl.clj")
  (rename-buffer "*inf-clj*"))

(defun wwai-cljs-repl ()
  (interactive)
  (inf-clojure "bin/run script/cljs_repl.clj")
  (rename-buffer "*inf-cljs*"))

(defvar inf-clojure-project nil)

(defmacro safe-wrap (fn &rest clean-up)
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (validate-setq retval (progn ,fn))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (validate-setq retval (cons 'exception (list ex)))))
         retval)
     ,@clean-up))

(defun clojure-custom-setup ()
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer))

  ;; inf-clojure config
  (when inf-clojure-project
    (progn
      (when (require 'inf-clojure nil 'noerror)
        (safe-wrap (inf-clojure-minor-mode)))
      (make-local-variable 'inf-clojure-buffer)
      (let ((ext (car (last (split-string (buffer-name (current-buffer)) "\\.")))))
        (if (equal ext "clj")
            (setq inf-clojure-buffer "*inf-clj*")
          (if (equal ext "cljs")
              (setq inf-clojure-buffer "*inf-cljs*")))))))

(use-package which-key :ensure t
  :config
  (which-key-mode)
  (validate-setq which-key-separator " ")
  (validate-setq which-key-idle-delay 0.6))

(use-package highlight-symbol :ensure t
  :init
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (add-hook 'prog-mode-hook 'highlight-symbol-nav-mode)
  :config
  (validate-setq highlight-symbol-idle-delay 0.2)
  (validate-setq highlight-symbol-on-navigation-p t)
  (validate-setq highlight-symbol-highlight-single-occurrence nil)
  (validate-setq highlight-symbol-occurrence-message '(explicit navigation)))

(use-package ag :ensure t
  :config
  (validate-setq ag-highlight-search t)
  (validate-setq ag-ignore-list '(".*.map" "resources")))

(use-package dumb-jump :ensure t
  :config
  (bind-keys*
   ("M-." . dumb-jump-go)
   ("C-c M-." . dumb-jump-go-other-window))
  (use-package s :ensure t)
  (validate-setq dumb-jump-selector 'ivy))

(use-package keyfreq :ensure t
  :init
  (validate-setq keyfreq-mode t)
  :config
  (validate-setq keyfreq-autosave-mode t))

;; Cast as a replacement to `highlight-symbol` but this seems more
;; useful as a tool for deliberately painting vars with faces to help
;; unsquirrel dense code and walking among all painted things, whereas
;; `highlight-symbol` highlights only the thing at point, and without
;; intervention.
(use-package symbol-overlay :ensure t
  :bind
  (("M-i" . symbol-overlay-put))
  :config
  (bind-keys
   :map symbol-overlay-map
   (unbind-key "M-u" symbol-overlay-map)
   (unbind-key "M-o" symbol-overlay-map)))

(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun decrement-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1- (string-to-number (match-string 0))))))
(global-set-key (kbd "C-c +") 'increment-number-at-point)
(global-set-key (kbd "C-c -") 'decrement-number-at-point)


(defun finder ()
  "Open the current working directory in finder."
  (interactive)
  (shell-command (concat "open " (shell-quote-argument default-directory))))


(defvar git-grep-switches "--extended-regexp -I -n"
  "Switches to pass to `git grep'.")

(defun git-grep-fullscreen (regexp &optional files dir confirm)
  (interactive
   (let* ((regexp (grep-read-regexp))
          (files (grep-read-files regexp))
          (files (if (string= "* .*" files) "*" files))
          (dir (ido-read-directory-name "Base directory: "
                                        nil default-directory t))
          (confirm (equal current-prefix-arg '(4))))
     (list regexp files dir confirm)))
  (let ((command (format "cd %s && git --no-pager grep %s %s -e %S -- '%s' "
                         dir
                         git-grep-switches
                         (if (s-lowercase? regexp) " --ignore-case" "")
                         regexp
                         files))
        (grep-use-null-device nil))
    (when confirm
      (validate-setq command (read-shell-command "Run git-grep: " command 'git-grep-history)))
    (window-configuration-to-register ?$)
    (grep command)
    (switch-to-buffer "*grep*")
    (delete-other-windows)
    (beginning-of-buffer)))

(use-package iflipb :ensure t
  :bind
  (("C-z"   . iflipb-next-buffer)
   ("C-M-z" . iflipb-previous-buffer))
  :config
  (validate-setq iflipb-ignore-buffers nil))

;; Would enable globally but messes up magit's status display for some reason.
;; https://bitbucket.org/adamsmd/digit-groups/issues/1/conflict-with-egg-mode
(use-package digit-groups :ensure t)

(use-package key-chord :ensure t
  :init
  (key-chord-mode 1)
  :config
  (key-chord-define-global "jk" 'avy-goto-char-timer)
  (key-chord-define-global ",." 'other-window)
  (key-chord-define-global "ZZ" 'save-buffer)
  (key-chord-define-global "zx" 'jump-to-register)
  (key-chord-define-global "ZX" 'window-configuration-to-register))

;; Don't display `\` when wrapping lines.
(set-display-table-slot standard-display-table 'wrap ?\ )

(use-package smooth-scrolling :ensure t
  :config (smooth-scrolling-mode 1))
(use-package centered-cursor-mode :ensure t
  :config (validate-setq centered-cursor-mode t))

(use-package google-translate :ensure t
  ;; `:demand` necessary to expose related setqs in `:config`.
  :demand t
  :bind (("C-c t" . google-translate-smooth-translate))
  :config
  (require 'google-translate-smooth-ui)
  (validate-setq google-translate-translation-directions-alist '(("da" . "en")))
  (validate-setq google-translate-output-destination 'echo-area))

(use-package align-cljlet :ensure t)
(use-package avy :ensure t)
(use-package beacon :ensure t)
(use-package browse-kill-ring :ensure t)
(use-package clojure-snippets :ensure t)
(use-package counsel :ensure t)
(use-package desktop :ensure t)
(use-package easy-kill :ensure t)
(use-package ffap :ensure t)
(use-package find-file-in-project :ensure t)
(use-package flymake-cursor :ensure t)
(use-package helm :ensure t)
(use-package hlinum :ensure t)
(use-package kill-ring-search :ensure t)
(use-package kpm-list :ensure t)
(use-package markdown-mode+ :ensure t)
(use-package nav :ensure t)
(use-package neotree :ensure t)
(use-package project-explorer :ensure t)
(use-package python-mode :ensure t)
(use-package rainbow-delimiters :ensure t)
(use-package rings :ensure t)
(use-package saveplace :ensure t)
(use-package undo-tree :ensure t)
(use-package yasnippet :ensure t)
(use-package savehist :ensure t
  :init
  (savehist-mode)
  :config
  (validate-setq savehist-file  (concat variable-files-dir ".savehist")))

;; Improvements over built-in `Help` facilities.
(use-package helpful :ensure t
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h t" . helpful-at-point))

