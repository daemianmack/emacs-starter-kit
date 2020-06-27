(use-package validate :ensure t)

(setq variable-files-dir (concat dotfiles-dir "var/"))

(set-register ?e (cons 'file (concat dotfiles-dir "experimental.el")))
(set-register ?n (cons 'file "~/Dropbox/docs/notes/notes.org"))
(set-register ?b (cons 'file "~/dotfiles/.bashrc"))
;; Performance improvement?
(validate-setq redisplay-dont-pause t)

(use-package display-line-numbers :ensure t
  :config
  (global-display-line-numbers-mode)
  (validate-setq display-line-numbers-width 3))

;; No longer using linum in favor of `display-line-numbers` mode, but
;; these forms may provide inspiration for controlling which buffers
;; use that mode.
;; (setq linum-disabled-modes-list
;;       '(cider-repl-mode grep-mode compilation-mode git-commit-mode help-mode))
;; (defun linum-on ()
;;   (unless (or (minibufferp) (member major-mode linum-disabled-modes-list))
;;     (linum-mode 1)))

;; Always load newest byte code.
(validate-setq load-prefer-newer t)

(validate-setq require-final-newline nil)

(validate-setq vc-follow-symlinks t)

(validate-setq custom-buffer-done-kill nil
               custom-buffer-verbose-help nil
               ;; Show real names in `custom` Interface.
               custom-unlispify-tag-names nil
               custom-unlispify-menu-entries nil)

;; Eliminate duplicates in the kill ring.
(validate-setq kill-do-not-save-duplicates t)

(use-package diminish :ensure t
  :config
  (diminish 'eldoc-mode)
  (diminish 'subword-mode))

;; Display registration of multi-key commands faster.
(validate-setq echo-keystrokes 0.1)

(use-package windmove
  :config
  ;; Integrate with tmux splits.
  (bind-keys
   ("S-<right>" . (lambda () (interactive) (util/windmove-emacs-or-tmux "right" "tmux select-pane -R")))
   ("S-<left>"  . (lambda () (interactive) (util/windmove-emacs-or-tmux "left"  "tmux select-pane -L")))
   ("S-<up>"    . (lambda () (interactive) (util/windmove-emacs-or-tmux "up"    "tmux select-pane -U")))
   ("S-<down>"  . (lambda () (interactive) (util/windmove-emacs-or-tmux "down"  "tmux select-pane -D")))))

;; Speed up display of very long lines.
(setq-default bidi-display-reordering nil)

(global-set-key (kbd "<f12>") 'bury-buffer)
(global-set-key (kbd "C-c C-i") 'bury-buffer)

(use-package flycheck :ensure t
  :init
  (use-package flycheck-clj-kondo :ensure t)
  (require 'flycheck-clj-kondo)
  ;; "This fn should have a docstring", etc.
  (validate-setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (validate-setq flycheck-check-syntax-automatically '(mode-enabled save))
  (global-flycheck-mode)
  :config
  ;; Warning about POSIX sh compatibility despite BASH shebang. (?)
  (validate-setq flycheck-shellcheck-excluded-warnings '("SC2039"))
  (validate-setq flycheck-indication-mode 'left-margin))

(use-package recentf :ensure t
  :init
  (recentf-mode 1)
  :config
  (validate-setq recentf-max-saved-items 100)
  (validate-setq recentf-save-file (concat variable-files-dir ".recentf")))

;; TODO ctrl-up, ctrl-down allow in-place preview via
;; `helm-follow-action-forward` etc but only from within
;; `helm-buffers-list` -- get this working from within
;; `helm-find-files` too.
(use-package helm :ensure t
  :init
  (helm-mode 1)
  (diminish 'helm-mode)
  :bind
  ("C-x C-b" . helm-buffers-list)
  ("H-b" . helm-mini)
  :config
  (validate-setq helm-mode-fuzzy-match t
                 helm-follow-mode-persistent t)
  ;; Use Mac OS X's Spotlight.
  (validate-setq helm-locate-command "mdfind -name %s %s"))

(require 'yasnippet)
(require 'clojure-snippets)
(yas-global-mode 1)
(diminish 'yas-minor-mode)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
;; Inherit local/ in a given mode by referencing it in that mode's .yas-parents.
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/local")
(yas-load-directory "~/.emacs.d/snippets")

(validate-setq desktop-restore-frames t)
(validate-setq desktop-restore-in-current-display t)
(validate-setq desktop-restore-forces-onscreen nil)

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

(validate-setq window-combination-resize t)

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

(add-hook 'find-file-hook 'util/mode-line-count-lines)
(add-hook 'after-save-hook 'util/mode-line-count-lines)
(add-hook 'after-revert-hook 'util/mode-line-count-lines)
(add-hook 'dired-after-readin-hook 'util/mode-line-count-lines)

(use-package smart-mode-line :ensure t
  :config
  (validate-setq sml/theme nil)
  (sml/setup)
  (validate-setq sml/name-width '(20 . 80))
  (validate-setq sml/outside-modified-char "‽")
  (validate-setq sml/modified-char "!")
  (setq-default mode-line-front-space
                '(:eval (concat (let ((str "%4l"))
                                  (when (and (not (buffer-modified-p)) util/mode-line-buffer-line-count)
                                    (setq str (concat str "/" util/mode-line-buffer-line-count)))
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

(use-package swoop :ensure t
  :bind (("C-c o"   . swoop)
         ("C-c O"   . swoop-multi)
         ("C-c M-o" . swoop-pcre-regexp))
  :config
  (global-set-key (kbd "C-c C-o") 'util/swoop-top-level-forms)
  (validate-setq swoop-window-split-direction: 'split-window-horizontally)
  (bind-keys
   :map swoop-map
   ("<up>"   . swoop-action-goto-line-prev)
   ("<down>" . swoop-action-goto-line-next)
   ("C-p"    . prev-history-element)
   ("C-n"    . next-history-element)))

(use-package ido :ensure t
  :config
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

  (add-hook 'ido-minibuffer-setup-hook 'util/ido-disable-line-truncation)
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

(use-package cider
  :ensure t
  :pin melpa-stable
  :bind (("C-c M-o" . cider-repl-clear-buffer-from-orbit)
         ("C-c d"   . cider-repl-reset)
         ("M-."     . clj-find-var-fallback))
  :config
  (use-package cider-eval-sexp-fu :ensure t
    :config
    (validate-setq eval-sexp-fu-flash-duration 0.05))
  (use-package clj-refactor       :ensure t
    :init
    (diminish 'clj-refactor-mode)
    (cljr-add-keybindings-with-prefix "C-c M-r")
    (validate-setq cljr-favor-prefix-notation nil)
    (validate-setq cljr-warn-on-eval nil)
    :config
    (validate-setq cljr-auto-clean-ns nil)
    (validate-setq cljr-auto-sort-ns nil))

  (add-hook 'cider-repl-mode-hook 'paredit-mode)

  (define-key cider-repl-mode-map (kbd "C-c M-o") 'cider-repl-clear-buffer)

  (validate-setq cider-print-fn 'zprint)
  (validate-setq cider-eval-spinner-type 'box-in-box)
  (validate-setq cider-font-lock-dynamically '(var))
  (validate-setq cider-overlays-use-font-lock t)
  (validate-setq cider-repl-history-file "~/.lein/cider-repl-history")
  (validate-setq cider-repl-result-prefix "")
  (validate-setq cider-repl-use-clojure-font-lock t)
  (validate-setq cider-show-error-buffer nil)
  (validate-setq cider-use-overlays nil)
  (validate-setq cider-repl-history-separator "────────────────────────────────────────────────────────")
  (validate-setq cider-repl-display-help-banner nil)
  (validate-setq cider-session-name-template "%j")
  (validate-setq nrepl-repl-buffer-name-template "*REPL %s*")

  (defun clj-find-var-fallback ()
    "Attempts to jump-to-definition of the symbol-at-point. If
  CIDER fails, or not available, falls back to dumb-jump"
    (interactive)
    (let ((var (cider-symbol-at-point)))
      (if (and (cider-connected-p) (cider-var-info var))
          (unless (eq 'symbol (type-of (cider-find-var nil var)))
            (dumb-jump-go))
        (dumb-jump-go))))

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
    (cider-repl-command "(dev/reset)")))

(use-package adaptive-wrap :ensure t)
(use-package visual-fill-column :ensure t)

(use-package projectile
  :ensure t
  :init
  (validate-setq projectile-cache-file (concat variable-files-dir "projectile.cache"))
  (validate-setq projectile-known-projects-file (concat variable-files-dir "projectile-bookmarks.eld"))
  :config
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (diminish 'projectile-mode) ;; Supplanted by smart-mode-line.
  (add-to-list 'projectile-globally-ignored-directories "resources/public/js")
  (add-to-list 'projectile-globally-ignored-file-suffixes ".log")
  (add-to-list 'projectile-globally-ignored-directories "js/compiled")
  :bind
  ("H-p" . projectile-switch-project)
  ("C-x f" . projectile-find-file)
  ("H-f" . projectile-find-file)
  ("H-s" . projectile-ripgrep))

(use-package magit
  :ensure t
;  :pin melpa-stable
  :bind (("C-x g" . magit-status)
         ("C-c f g" . magit-log-buffer-file)
         ("C-c b" . magit-browse-pull-request))
  :config
  ;; Avoid version skew that breaks Magit's git-rebase-mode.
  (use-package with-editor :ensure t)
  (use-package ido-completing-read+ :ensure t)
  (validate-setq transient-history-file (concat variable-files-dir "transient-history.el"))
  (validate-setq magit-completing-read-function 'magit-ido-completing-read)
  (validate-setq magit-diff-refine-hunk 'all)
  (validate-setq magit-process-popup-time 10)
  (validate-setq magit-auto-revert-mode 't)
  (validate-setq magit-repository-directories '(("/Users/daemianmack/dev" . 0)))
  (add-hook 'ido-setup-hook
            (lambda ()
              (define-key ido-completion-map
                (kbd "C-x g") 'ido-enter-magit-status))))

(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'prog-mode-hook 'util/add-watchwords)
(add-hook 'prog-mode-hook 'util/comment-auto-fill)

(use-package rainbow-delimiters :ensure t
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(add-hook 'lisp-mode-hook 'util/lisp-mode-setup)
(add-hook 'emacs-lisp-mode-hook 'util/lisp-mode-setup)

(use-package clojure-mode
  :ensure t
  :bind (("C-c ;" . comment/comment-or-uncomment-sexp))
  :config
  (load (concat dotfiles-dir "comment-or-uncomment"))
  (diminish 'clojure-mode "clj")
  (diminish 'clojurescript-mode "cljs")
  (bind-keys :map clojure-mode-map ("C-c C-r n c" . util/clojure-copy-ns))
  (add-to-list 'auto-mode-alist '("\\.clj" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljs" . clojurescript-mode))
  (add-to-list 'auto-mode-alist '("\\.cljc" . clojurec-mode))
  (add-to-list 'auto-mode-alist '("\\.edn" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.boot" . clojure-mode))
  (add-hook 'clojure-mode-hook 'util/lisp-mode-setup)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  (add-hook 'clojure-mode-hook 'util/no-final-newline)
  (add-hook 'cider-repl-mode-hook 'util/lisp-mode-setup)
  ;; Indent Clojure's `comment` form like a defun -- don't line up non-first-line args under first-line args.
  (put-clojure-indent 'comment 'defun)
  ;; Font-lock Datomic's logic vars (`?user-id`).
  (font-lock-add-keywords 'clojure-mode
                          '(("?[:alnum:][[:alnum:][:punct:]]+" . font-lock-constant-face)))
  ;; This causes CLJS buffer errors to return REPL control more quickly.
  (validate-setq max-lisp-eval-depth 20000)
  (validate-setq clojure-indent-style '(quote always-align)))

(use-package whitespace
  :ensure t
  :config
  (validate-setq whitespace-style '(face trailing tabs))
  (diminish 'whitespace-mode))

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
  (diminish 'ivy-mode)
  (validate-setq ivy-extra-directories nil)
  (validate-setq enable-recursive-minibuffers t)
  (validate-setq ivy-virtual-abbreviate 'full)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-x i") 'counsel-imenu)
  (bind-keys
   :map swiper-map
   ("C-r" . ivy-previous-line)))

(use-package paredit :ensure t
  :config
  (bind-keys :map paredit-mode-map
             ("C-J" . paredit-newline-in-place))
  (diminish 'paredit-mode " )( ") ;; ¯\_(ツ)_/¯
  (defun paredit-newline-in-place()
    (interactive)
    (progn (paredit-newline)
           (forward-line -1))))

(use-package pinentry
  :ensure t
  :config
  (setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))
  (pinentry-start))

(use-package yaml-mode
  :ensure t)


;; This has special sauce to avoid a hang on shutdown due to pcache.
(add-to-list 'load-path "/Users/daemianmack/.emacs.d/elpa/back-button-0.6.6")

(use-package back-button
  :ensure t
  :config
  (back-button-mode 1)
  (diminish 'back-button-mode " ⟲ "))

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
  (use-package org-cliplink :ensure t)
  (require 'ob-clojure)
  (require 'cider)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)
     (clojure . t)
     (sql .t)
     (dot .t)))
  (unbind-key "S-<up>" org-mode-map)
  (unbind-key "S-<down>" org-mode-map)
  (unbind-key "S-<right>" org-mode-map)
  (unbind-key "S-<left>" org-mode-map)
  (unbind-key "C-c =" org-mode-map)
  (setq org-src-fontify-natively t)
  (setq org-hide-leading-stars t)
  (setq org-return-follows-link t)
  (setq org-odd-levels-only t)
  ;; Show all empty lines when collapsed.
  (setq org-cycle-separator-lines -1)
  (setq org-cycle-global-at-bob t)
  (setq org-yank-folded-subtrees nil)
  (setq org-yank-adjusted-subtrees t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-clojure-backend 'cider)
  (setq org-babel-ditaa-java-cmd "java -Dapple.awt.UIElement=true")
  (setq org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.9/libexec/ditaa0_9.jar")
  (make-face 'org-inflight-face)
  (setq org-todo-keyword-faces '(("DOING" . org-inflight-face)))
  (setq org-capture-templates
        '(("x"
           "Exercise note"
           item
           (file "/Users/daemian/Dropbox/docs/notes/exercise-notes.org")
           ""
           :unnarrowed t)))
  :bind (:map org-mode-map
              ;; These top two keybindings require OSX Mission Control
              ;; relinquish the bindings.
              ("<C-right>" . org-shiftright) ;; Ctrl+<arrow> to cycle TODO states.
              ("<C-left>"  . org-shiftleft)
              ("<C-up>"    . org-shiftup)
              ("<C-down>"  . org-shiftdown)))

(use-package which-key :ensure t
  :config
  (which-key-mode)
  (diminish 'which-key-mode)
  (validate-setq which-key-separator " ")
  (validate-setq which-key-sort-order 'which-key-prefix-then-key-order)
  (validate-setq which-key-idle-delay 0.6)
  (validate-setq which-key-description-replacement-alist
                 '(("Prefix Command" . "prefix")
                   ;; Lambdas
                   ("\\`\\?\\?\\'"   . "λ")
                   ;; Drop/shorten package prefixes
                   ("projectile-"    . "proj-"))))

(use-package ag :ensure t
  :config
  (validate-setq ag-highlight-search t)
  (validate-setq ag-context-lines 3)
  ;; Have to use `ag` directly -- `project-ag` uses gitignore instead. :(
  (validate-setq ag-ignore-list '(".*.map" "resources" "front.*" "**.log" "resources/public/js/compiled"))
  (bind-keys*
   ;; Insert into `projectile` key map.
   ("C-c p s a" . ag-project))
  ;; Clobber ag's buffer-name display tactic in absence of a proper customize option.
  (defun ag/buffer-name (search-string directory regexp)
    "Return a buffer name formatted according to ag.el conventions."
    (cond
     (ag-reuse-buffers "*ag search*")
     (regexp (format "*ag search regexp:%s dir:%s*" search-string directory))
     ;; Much shorter format than stock.
     ;; (:else (format "*ag search text:%s dir:%s*" search-string directory))
     (:else (format "*ag %s*" search-string)))))

(use-package dumb-jump :ensure t
  :config
  (bind-keys*
   ("M-."     . clj-find-var-fallback)
   ("C-c M-." . dumb-jump-go-other-window))
  (use-package s :ensure t)
  (validate-setq dumb-jump-selector 'ivy))

(use-package keyfreq :ensure t
  :init
  (validate-setq keyfreq-mode t)
  :config
  (validate-setq keyfreq-autosave-mode t))

(use-package symbol-overlay :ensure t
  :bind
  (("M-i" . symbol-overlay-put) ;; Enter 'mode' where subcommands available.
   ;; Globalize nav subcommands.
   ("M-n" . symbol-overlay-jump-next)
   ("M-p" . symbol-overlay-jump-prev))
  :config
  ;; TODO Which one of these do we need to apply this mode to every
  ;; prog/text buffer?
  (add-hook 'after-init-hook 'symbol-overlay-mode)
  (add-hook 'prog-mode-hook 'symbol-overlay-mode)
  (validate-setq symbol-overlay-idle-time 0.2)

  ;; TODO Correct this. The intent: when point is on `:foo'` highlight
  ;; all `foo`, navigate same.
  (defun my-symbol-overlay-get-symbol (&optional string noerror)
    "Get the symbol at point.
If STRING is non-nil, `regexp-quote' STRING rather than the symbol.
If NOERROR is non-nil, just return nil when no symbol is found."
    (let ((symbol (or string (thing-at-point 'symbol))))
      (if symbol (regexp-quote (replace-regexp-in-string "[:]" "" symbol))
        (unless noerror (user-error "No symbol at point"))))))

(defvar git-grep-switches "--extended-regexp -I -n"
  "Switches to pass to `git grep'.")

(use-package iflipb :ensure t
  :bind
  (("C-z"   . iflipb-next-buffer)
   ("C-M-z" . iflipb-previous-buffer))
  :config
  (validate-setq iflipb-ignore-buffers nil))

(use-package key-chord :ensure t
  :init
  (key-chord-mode 1)
  :config
  ;; These next two handy when browsing search results generated by
  ;; compilation-mode -- can navigate through hits without having to
  ;; enter search results buffer.
  (key-chord-define-global "jk" 'next-error)
  (key-chord-define-global "JK" 'previous-error)
  (key-chord-define-global " f" 'avy-goto-char-timer)
  (key-chord-define-global " n" 'neotree-toggle)
  (key-chord-define-global ",." 'other-window)
  (key-chord-define-global "ZZ" 'save-buffer)
  (key-chord-define-global "zx" 'jump-to-register)
  (key-chord-define-global "ZX" 'window-configuration-to-register)
  ;; Not sure optimal values for these settings
  ;; We want to NOT delay typing, and NOT mis-interpret pastes as
  ;; commands, while keeping commands easy for human fingers to
  ;; express within the timeout.
  (validate-setq key-chord-one-key-delay 0.1)
  (validate-setq key-chord-two-keys-delay 0.02))

;; Don't display `\` when wrapping lines.
(set-display-table-slot standard-display-table 'wrap ?\ )

(use-package logview :ensure t
  :config (validate-setq datetime-timezone 'US/Eastern)
  ;; :config
  ;; TODO This is the custom.el version for what I want but
  ;; validate-setq doesn't like its format when porting it here.
  ;; (validate-setq logview-additional-submodes
  ;;                '(("mine-strict"
  ;;                  (format . "TIMESTAMP LEVEL  THREAD - ")
  ;;                  (levels . "SLF4J"))
  ;;                 ("mine-dmot"
  ;;                  (format . "TIMESTAMP LEVEL   THREAD MESSAGE")
  ;;                  (levels . "SLF4J")
  ;;                  (timestamp "ISO 8601 datetime + millis")
  ;;                  (aliases))))
  )


(use-package google-translate :ensure t
  ;; `:demand` necessary to expose related setqs in `:config`.
  :demand t
  :bind (("C-c t" . google-translate-smooth-translate))
  :config
  (require 'google-translate-smooth-ui)
  (validate-setq google-translate-translation-directions-alist '(("pt" . "en")))
  (validate-setq google-translate-output-destination 'echo-area)
  ;; Temporary workaround: https://github.com/atykhonov/google-translate/issues/52#issuecomment-423870290
  (when (and (string-match "0.11.14"
                           (google-translate-version))
             (>= (time-to-seconds)
                 (time-to-seconds
                  (encode-time 0 0 0 23 9 2018))))
    (defun google-translate--get-b-d1 ()
      ;; TKK='427110.1469889687'
      (list 427110 1469889687)))
  ;; Hack around `(args-out-of-range [] 1)` https://github.com/atykhonov/google-translate/issues/98
  ;; Why is this package so hacky?
  (defun google-translate-json-suggestion (json)
    "Retrieve from JSON (which returns by the
`google-translate-request' function) suggestion. This function
does matter when translating misspelled word. So instead of
translation it is possible to get suggestion."
    (let ((info (aref json 7)))
      (if (and info (> (length info) 0))
          (aref info 1)
        nil)))  )

(use-package browse-kill-ring :ensure t
  :config
  (validate-setq browse-kill-ring-separator "──")
  (validate-setq kill-ring-max 500))

(use-package helm :ensure t
  :config (validate-setq helm-split-window-default-side 'left)
  (validate-setq helm-full-frame t)
  :bind (([remap switch-to-buffer] . helm-mini)))

(use-package desktop :ensure t
  :config
  (validate-setq desktop-auto-save-timeout 60))

(use-package align-cljlet :ensure t)
(use-package avy :ensure t)
(use-package beacon :ensure t
  :init (diminish 'beacon-mode)
  :config (validate-setq beacon-color "#8700d7"))
(use-package clojure-snippets :ensure t)
(use-package counsel :ensure t)
(use-package desktop :ensure t)
(use-package easy-kill :ensure t)
(use-package ffap :ensure t)
(use-package find-file-in-project :ensure t)
(use-package flymake-cursor :ensure t);;todo
(use-package markdown-mode+ :ensure t)
(use-package neotree :ensure t)
(use-package project-explorer :ensure t)
(use-package python-mode :ensure t)
(use-package rainbow-delimiters :ensure t)
(use-package saveplace :ensure t)
(use-package undo-tree :ensure t
  :config (diminish 'undo-tree-mode)
  (validate-setq undo-tree-visualizer-timestamps t))
(use-package yasnippet :ensure t)

(use-package savehist :ensure t
  :init
  (savehist-mode)
  :config
  (validate-setq savehist-file (concat variable-files-dir ".savehist")))

(validate-setq locale-coding-system 'utf-8)
(set-language-environment 'utf-8)
;; For old Carbon emacs on OS X only
(set-keyboard-coding-system 'utf-8-mac)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-locale-environment "en.UTF-8")
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)


;; These settings are meant to be useful for midnight mode to run
;; automatically but also to allow manually running
;; `clean-buffer-list` when annoyed at buffer sprawl.
(use-package midnight-mode
  :no-require t
  :init
  (midnight-mode 1)
  (validate-setq midnight-period 7200) ;; Auto-run frequently -- the "day" occurs every 2 hours.
  (midnight-delay-set 'midnight-delay 5) ;; Kill at this offset in the "day".
  (validate-setq clean-buffer-list-delay-special 600) ;; Don't kill buffers displayed in the last n seconds.
  (validate-setq clean-buffer-list-kill-regexps
                 '("\\`magit.*\\'"
                   "\\`\\*helm.*\\*\\'"
                   "\\`\\*info.*\\'"
                   "\\`\\*ag .*\\*\\'"
                   "\\`\\*helpful .*\\*\\'"
                   "\\`\\*Occur\\*\\'"
                   "\\`\\*cider-error\\*\\'"
                   "\\`\\*cider-result\\*\\'"
                   "\\`\\*edn\\*\\'"
                   "\\`\\*Help\\*\\'"
                   "\\`\\*Shell Command Output\\*\\'"
                   "\\`\\*Backtrace\\*\\'"
                   "\\`\\*Packages\\*\\'"
                   "\\`\\*Warnings\\*\\'"
                   "\\`\\*Annotate.*\\*\\'"
                   ;; `magit-diff-visit-file` buffers, e.g, `CHANGELOG.md.~master~3~'
                   "\\`.*~.*~.*~\\'"
                   "\\`dev.clj\\'"))
  :bind (("C-x M-k" . util/clean-buffer-list-now)))

;; Improvements over built-in `Help` facilities.
(use-package helpful :ensure t
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h t" . helpful-at-point))

(use-package dockerfile-mode :ensure t)

(use-package git-link :ensure t
  :config
  (setq-default git-link-default-branch "master")
  (validate-setq git-link-open-in-browser t)
  (defalias 'gh/visit-file 'git-link))

(use-package google-this :ensure t
  :config
  (google-this-mode 1)
  (diminish 'google-this-mode)
  :bind ("C-c T" . google-this))

(use-package my-buffers
  :no-require t
  :bind (("C-x C-k" . util/my-kill-buffer))
  :init
  (add-to-list 'kill-buffer-query-functions 'util/do-not-kill-important-buffers))

(use-package company :ensure t
  :config
  (validate-setq company-tooltip-minimum company-tooltip-limit)
  (validate-setq company-frontends '(company-pseudo-tooltip-frontend))
  (validate-setq company-tooltip-align-annotations t))

(use-package cus-edit
  :config
  (validate-setq custom-search-field nil
                 custom-buffer-done-kill nil
                 custom-buffer-verbose-help nil
                 ;; Show real names in `custom` Interface.
                 custom-unlispify-tag-names nil
                 custom-unlispify-menu-entries nil))

(use-package diff-hl                    ; Highlight hunks in fringe
  :ensure t
  :init
  ;; Highlight changes to the current file in the fringe
  (global-diff-hl-mode)
  ;; Highlight changed files in the fringe of Dired
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

  ;; Fall back to the display margin, if the fringe is unavailable
  (unless (display-graphic-p)
    (diff-hl-margin-mode))

  ;; Refresh diff-hl after Magit operations
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  ;; :config (add-hook 'vc-checkin-hook 'diff-hl-update)
  (validate-setq diff-hl-side 'left))

(use-package gist
  :ensure t
  ;; :bind (("C-c g g l" . gist-list)
  ;;        ("C-c g g b" . gist-region-or-buffer))
  :init (use-package gh :ensure t)
  :config (validate-setq gist-view-gist t)
  )

(use-package ialign
  :ensure t
  :bind (("C-c l" . ialign)))

(save-place-mode)
(validate-setq save-place-file (concat variable-files-dir ".emacs-places"))

(use-package iedit :ensure t)

(require 'zone)

(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :config
  ;; Colourful columns.
  (use-package diredfl
    :ensure t
    :config
    (diredfl-global-mode 1))
  (use-package dired-git-info
    :ensure t
    :bind (:map dired-mode-map
                (")" . dired-git-info-mode))))

;; Configure `ispell` which will use hunspell's dictionary at ~/.hunspell_en_US
(setenv
 "DICPATH"
 (concat (getenv "HOME") "/Library/Spelling"))
(setq ispell-program-name "/usr/local/bin/hunspell")

(setq org-startup-folded 'showeverything)
(use-package company
  :ensure t
  :demand t
  :bind (("M-<tab>" . company-complete))
  :init (progn
          ;; start company mode now and run it in all modes
          (add-hook 'after-init-hook 'global-company-mode)
          (setq company-idle-delay nil)

          ;; use TAB to indent or start completion
          ;; (global-set-key (kbd "TAB") 'company-indent-or-complete-common)
          ;; don't force a match so we can type some other stuff and not have
          ;; company block my typing
          (setq company-require-match nil)
          ;; show completion number
          (setq company-show-numbers t)
          ;; look in comments and strings
          (setq company-dabbrev-code-everywhere t))
  :config (progn
            ;; complete things in my current buffer using company-dabbrev
            ;; http://emacs.stackexchange.com/questions/15246/how-add-company-dabbrev-to-the-company-completion-popup
            (add-to-list 'company-backends '(company-capf :with company-dabbrev))
            (add-to-list 'company-backends '(company-capf :with company-dabbrev-code))
            ;; Sort the company suggestions by preferring things within the
            ;; current buffer before stuff outside of this buffer
            (setq company-transformers (quote (company-sort-by-occurrence)))))

(use-package restclient :ensure t)

(use-package multiple-cursors :ensure t
  :bind
  ("C-c <" . mc/mark-previous-like-this-word)
  ("C-c >" . mc/mark-next-like-this-word))

(use-package helm-swoop :ensure t
  :bind
  ("C-s" . helm-swoop)
  :config
  (validate-setq helm-swoop-speed-or-color t))

(use-package ripgrep :ensure t)

(use-package xref :ensure t
  :config
  (bind-keys
   ("M-," . my-xref-return))
  (defun xref-pop-marker-stack ()
    "Pop back to where \\[xref-find-definitions] was last invoked."
    (interactive)
    (let ((ring xref--marker-ring))
      (when (ring-empty-p ring)
        (user-error "Marker stack is empty"))
      (let ((marker (ring-remove ring 0)))
        (switch-to-buffer (or (marker-buffer marker)
                              (user-error "The marked buffer has been deleted")))
        (goto-char (marker-position marker))
        (set-marker marker nil nil))))

  (defun my-xref-return ()
    "Before returning from an xref jump, kill the jumped-to buffer.:"
    (interactive)
    ;; TODO Only kill this buffer if this buffer is not same as previous
    ;; buffer, as when when we jump to definition and end up in same file.
    (kill-this-buffer)
    (xref-pop-marker-stack)))

(use-package annotate :ensure t
  :config
  (validate-setq annotate-annotation-column 25))

(use-package lsp-mode
  :ensure t
  :defer t
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp))
  :config
  (diminish 'lsp-mode "lsp")
  (validate-setq read-process-output-max (* 1024 1024))
  (validate-setq lsp-prefer-capf t)
  (setenv "PATH" (concat
                   "/usr/local/bin" path-separator
                   (getenv "PATH")))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
     (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  (setq lsp-enable-indentation nil
        lsp-clojure-server-command '("bash" "-c" "clojure-lsp")
        lsp-ui-doc-delay 3
        lsp-enable-symbol-highlighting nil
        lsp-session-file (concat variable-files-dir ".lsp-session-v1"))
  (define-key lsp-mode-map (kbd "H-l") lsp-command-map)
  (define-key lsp-mode-map (kbd "H-l i l") 'lsp-clojure-introduce-let)
  (define-key lsp-mode-map (kbd "H-l m l") 'lsp-clojure-move-to-let)
  (define-key lsp-mode-map (kbd "H-.") 'lsp-find-definition)
  ;; Don't display diag; clj-kondo does a better job.
  (validate-setq lsp-diagnostic-package :none)
  (validate-setq max-specpdl-size 13000)
  (validate-setq flycheck-checker-error-threshold 4000)
  (validate-setq lsp-file-watch-threshold 5000)
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.shadow-cljs$")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.clj-kondo$")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]resources$"))

;; Separate file for lexical binding.
;; TODO Will that setting in this file mess anything up?
(load (concat dotfiles-dir "offscreen-paren-matching"))

;; Keybindings for tab-bar-mode.
;; tab-bar-mode should be called tabset-mode;
;; Each "tab" is a view onto some number of buffers in a given layout
;; and we can cycle through these layouts via navigating "tabs".
(global-set-key (kbd "H-<left>")  'tab-previous)
(global-set-key (kbd "H-<right>")  'tab-next)
(global-set-key (kbd "H-r")  'tab-rename)
(validate-setq tab-bar-close-button-show nil)
(validate-setq tab-bar-new-button-show nil)

(use-package dbc :ensure t
  :config
  ;; Intent: nix situation where a buffer is set to side, but
  ;; then I'm prevented from killing *other* windows due to
  ;; "delete-window: Attempt to delete main window of frame" errors.
  ;; Unsure if undesirable side-effects.
  (validate-setq ignore-window-parameters t)
  (dbc-add-ruleset "right" '((display-buffer-reuse-window display-buffer-in-side-window) . ((side . right) (window-width . 0.4))))
  (dbc-add-rule "right" "help" :newname "\\*helpful.*"))

(use-package side-notes :ensure t
  :config
  (setq side-notes-display-alist '((side . right) (window-width . 60)))
  (defun side-notes-toggle-main-notes ()
    (interactive)
    (let ((side-notes-secondary-file "/Users/daemianmack/Dropbox/docs/notes/notes.org")
          (side-notes-display-alist '((side . right) (window-width . 100))))
      (side-notes-toggle-notes)))
  (global-set-key (kbd "H-[") 'side-notes-toggle-main-notes)
  (global-set-key (kbd "H-]") 'side-notes-toggle-notes))

(use-package origami :ensure t
  :config
  (global-origami-mode)
  (global-set-key (kbd "<backtab>") 'origami-toggle-node)
  (global-set-key (kbd "C-c <backtab>") 'origami-open-node-recursively))
