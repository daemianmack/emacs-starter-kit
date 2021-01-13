(use-package validate :ensure t)

;; For the love \ of all that is holy
;; 2c mode \ go away forever
(global-set-key [f2] nil)

(setq variable-files-dir (concat dotfiles-dir "var/"))

(set-register ?e (cons 'file (concat dotfiles-dir "experimental.el")))
(set-register ?n (cons 'file "~/Dropbox/docs/notes/notes.org"))
(set-register ?b (cons 'file "~/dotfiles/.bashrc"))
;; Performance improvement?
(validate-setq redisplay-dont-pause t)

(use-package display-line-numbers :ensure t
  :config
  (validate-setq display-line-numbers-width 3)
  :hook ((prog-mode text-mode conf-mode) . display-line-numbers-mode))

;; circumvents a couple startup optimizations
;; by eager-loading a couple packages associated with text modes, like flyspell
(setq initial-major-mode 'fundamental-mode)

;; No longer using linum in favor of `display-line-numbers` mode, but
;; these forms may provide inspiration for controlling which buffers
;; use that mode.
;; (setq linum-disabled-modes-list
;;       '(cider-repl-mode grep-mode compilation-mode git-commit-mode help-mode))
;; (defun linum-on ()
;;   (unless (or (minibufferp) (member major-mode linum-disabled-modes-list))
;;     (linum-mode 1)))

(validate-setq scroll-conservatively 101)

(setq fast-but-imprecise-scrolling t)

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))

(setq compilation-always-kill t ; kill compilation process before starting another
      compilation-ask-about-save nil   ; save all buffers on `compile'
      compilation-scroll-output 'first-error
      next-error-recenter '(4))

(use-package highlight-numbers :ensure t
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))


;; Show absolute line numbers for narrowed regions makes it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(setq-default display-line-numbers-widen t)

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
  (diminish 'gcmh-mode)
  (diminish 'auto-fill-mode))

;; Display registration of multi-key commands faster.
(validate-setq echo-keystrokes 0.1)

(use-package windmove
  :config
  ;; Integrate with tmux splits.
  ;; Giving these names allows symbol-seeking things like
  ;; `iflipb-first-iflipb-buffer-switch-command` to not barf.
  (defun pane-right () (interactive) (util/windmove-emacs-or-tmux "right" "-R"))
  (defun pane-left  () (interactive) (util/windmove-emacs-or-tmux "left"  "-L"))
  (defun pane-up    () (interactive) (util/windmove-emacs-or-tmux "up"    "-U"))
  (defun pane-down  () (interactive) (util/windmove-emacs-or-tmux "down"  "-D"))
  (bind-keys
   ("S-<right>" . pane-right)
   ("S-<left>"  . pane-left)
   ("S-<up>"    . pane-up)
   ("S-<down>" .  pane-down)))

;; Speed up display of very long lines.
(setq-default bidi-display-reordering nil)

(use-package flycheck :ensure t
  :config
  (use-package flycheck-clj-kondo :ensure t)
  (require 'flycheck-clj-kondo)
  ;; "This fn should have a docstring", etc.
  (validate-setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (validate-setq flycheck-check-syntax-automatically '(mode-enabled save))
  (global-flycheck-mode)

  ;; Warning about POSIX sh compatibility despite BASH shebang. (?)
  (validate-setq flycheck-shellcheck-excluded-warnings '("SC2039"))
  (validate-setq flycheck-indication-mode 'left-margin))

(use-package recentf :ensure t
  :config
  (recentf-mode 1)
  (validate-setq recentf-max-saved-items 100)
  (validate-setq recentf-save-file (concat variable-files-dir ".recentf"))
  ;; Persist to disk more frequently.
  (add-hook 'midnight-hook 'recentf-save-list))

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
  ("H-<up>" . helm-all-mark-rings)
  :config
  (validate-setq helm-mode-fuzzy-match t
                 helm-follow-mode-persistent t)
  ;; Use Mac OS X's Spotlight.
  (validate-setq helm-locate-command "mdfind -name %s %s"))

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
         ("M-."     . clj-find-var-fallback)
         ;; Configure iTerm to send ctrl+ret as escape sequence `^[[[`
         ;;   and shift+ret as escape sequence `^[ [{`
         :map cider-repl-mode-map
         ("M-[ [" . cider-repl-newline-and-indent)
         ("M-[ {" . cider-repl-newline-and-indent))
  :config
  (use-package cider-eval-sexp-fu :ensure t
    :config
    (validate-setq eval-sexp-fu-flash-duration 0.05))
  (use-package clj-refactor       :ensure t
    :config
    (diminish 'clj-refactor-mode)
    (cljr-add-keybindings-with-prefix "C-c M-r")
    (validate-setq cljr-favor-prefix-notation nil)
    (validate-setq cljr-warn-on-eval nil)
    (validate-setq cljr-auto-clean-ns nil)
    (validate-setq cljr-auto-sort-ns nil))

  (add-hook 'cider-repl-mode-hook 'paredit-mode)

  (define-key cider-repl-mode-map (kbd "C-c M-o") 'cider-repl-clear-buffer)

  (validate-setq cider-print-fn 'pprint)
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
        (dumb-jump-go))
      (recenter)))

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

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (validate-setq projectile-cache-file (concat variable-files-dir "projectile.cache"))
  (validate-setq projectile-known-projects-file (concat variable-files-dir "projectile-bookmarks.eld"))
  (validate-setq projectile-completion-system 'helm)
  (validate-setq projectile-switch-project-action 'helm-projectile)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (diminish 'projectile-mode) ;; Supplanted by smart-mode-line.
  (add-to-list 'projectile-globally-ignored-directories "resources/public/js")
  (add-to-list 'projectile-globally-ignored-file-suffixes ".log")
  (add-to-list 'projectile-globally-ignored-directories "js/compiled")
  (when (require 'magit nil t)
    (mapc #'projectile-add-known-project
          (mapcar #'file-name-as-directory (magit-list-repos)))
    (projectile-save-known-projects))
  :bind
  ("H-p" . projectile-switch-project)
  ("C-x f" . projectile-find-file)
  ("H-f" . projectile-find-file)
  ("H-s" . projectile-ripgrep))

(use-package magit
  :defer t
  :ensure t
;  :pin melpa-stable
  :bind (("C-x g" . magit-status)
         ("C-c f g" . magit-log-buffer-file)
         ("C-c b" . magit-browse-pull-request))
  :config
  ;; Avoid version skew that breaks Magit's git-rebase-mode.
  (use-package with-editor :ensure t)
  (validate-setq transient-history-file (concat variable-files-dir "transient-history.el"))
  (validate-setq magit-completing-read-function 'magit-ido-completing-read)
  (validate-setq magit-diff-refine-hunk 'all)
  (validate-setq magit-process-popup-time 10)
  (diminish 'auto-revert-mode)
  (validate-setq magit-auto-revert-mode 't)
  (validate-setq magit-repository-directories `((,(expand-file-name "~/dev") . 3)))
  (add-hook 'ido-setup-hook
            (lambda ()
              (define-key ido-completion-map
                (kbd "C-x g") 'ido-enter-magit-status))))

(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'prog-mode-hook 'util/add-watchwords)
(add-hook 'prog-mode-hook 'util/comment-auto-fill)

(use-package rainbow-delimiters :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

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
  (use-package counsel :ensure t)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-x i") 'counsel-imenu)
  (bind-keys
   :map swiper-map
   ("C-r" . ivy-previous-line)))

;; `paredit-forward-slurp-sexp`,`paredit-forward-barf-sexp` commands
;; use ctrl-right-arrow and ctrl-left-arrow respectively; disable OSX'
;; mission control shortcuts to free these up.
(use-package paredit :ensure t
  :config
  (diminish 'paredit-mode " )( ") ;; ¯\_(ツ)_/¯
  )

(use-package pinentry
  :ensure t
  :config
  (setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))
  (pinentry-start))

(use-package yaml-mode
  :ensure t)


;; This has special sauce to avoid a hang on shutdown due to pcache.
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/back-button-0.6.6"))

(use-package back-button
  :defer t
  :ensure t
  :init (back-button-mode 1)
  :config
  (validate-setq back-button-local-backward-keystrokes  '("C-x <left>"))
  (validate-setq back-button-local-forward-keystrokes   '("C-x <right>"))
  (validate-setq back-button-global-backward-keystrokes '("C-x <C-left>"))
  (validate-setq back-button-global-forward-keystrokes  '("C-x <C-right>"))
  (diminish 'back-button-mode " ⟲ ")
  (advice-add 'back-button-pop-local-mark :after #'recenter)
  (advice-add 'pop-global-mark :after #'recenter)
  (validate-setq back-button-smartrep-prefix ""))

(use-package smartrep :ensure t
  :config
  (smartrep-define-key global-map "C-x"
    '(("," . back-button-local-backward)
      ("." . back-button-local-forward)
      ( " { "  . shrink-window-horizontally)
      ( " } "  . enlarge-window-horizontally))))

(add-hook 'window-configuration-change-hook 'recenter)
(add-hook 'window-buffer-change-functions 'recenter)

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
;;   ctrl+RET         [[[
(use-package org
  :defer 5
  :bind (:map org-mode-map
              ;; These top two keybindings require OSX Mission Control
              ;; relinquish the bindings.
              ("<C-right>" . org-shiftright) ;; Ctrl+<arrow> to cycle TODO states.
              ("<C-left>"  . org-shiftleft)
              ("<C-up>"    . org-shiftup)
              ("<C-down>"  . org-shiftdown)
              ("C-c ,"     . org-time-stamp-inactive)
              ;; e.g. ctrl+RET on a heading to insert a child heading
              ("M-[ ["      . org-insert-subheading))
  :config
  (use-package org-cliplink :ensure t)
  ;; TODO Get this licensed/debugged, put on MELPA
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/org-pretty-table"))
  (use-package org-pretty-table)
  (use-package org-recent-headings :ensure t
    :config (org-recent-headings-mode)
    (add-to-list 'helm-mini-default-sources 'helm-org-recent-headings-source t)
    (use-package helm-org-recent-headings :ensure t))
  (use-package org-superstar :ensure t
    :init (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
    :config
    ;; Stop cycling bullets to emphasize hierarchy of headlines.
    (setq org-superstar-cycle-headline-bullets nil)
    ;; Hide away leading stars on terminal.
    (setq org-superstar-leading-fallback ?\s)
    (setq org-superstar-headline-bullets-list
          '("●" "■" "○" "□" "∙" "▫""⁂"))
    (org-superstar-restart))
  (require 'ob-clojure)
  (require 'cider)
  (unbind-key "S-<up>" org-mode-map)
  (unbind-key "S-<down>" org-mode-map)
  (unbind-key "S-<right>" org-mode-map)
  (unbind-key "S-<left>" org-mode-map)
  (unbind-key "C-c =" org-mode-map)
  (progn
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((ditaa . t)
       (clojure . t)
       (sql .t)
       (dot .t)
       (shell .t)))
    (setq org-src-fontify-natively t)
    (setq org-ellipsis "⬎")
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
             (file (expand-file-name "~/Dropbox/docs/notes/exercise-notes.org"))
             ""
             :unnarrowed t)))
    (setq org-agenda-files (list (expand-file-name "~/Dropbox/docs/notes/")))
    (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
    (setq org-outline-path-complete-in-steps nil)
    (setq org-time-stamp-custom-formats '("<%m/%d/%y %a>" . "[%a %b %d %H:%M]"))
    (setq org-attach-id-dir (concat dotfiles-dir "org-mode-data"))
    (setq org-startup-folded 'content)))

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
  :hook  (prog-mode . symbol-overlay-mode)
  :bind
  (("M-i" . symbol-overlay-put) ;; Enter 'mode' where subcommands available.
   ;; Globalize nav subcommands.
   ("M-n" . my-symbol-overlay-jump-next)
   ("M-p" . my-symbol-overlay-jump-prev))
  :config
  (diminish 'symbol-overlay-mode)
  (validate-setq symbol-overlay-idle-time 0.2)
  ;; TODO Use advice to do this.
  (defun my-symbol-overlay-jump-next ()
    (interactive)
    (symbol-overlay-jump-next)
    (recenter))
  (defun my-symbol-overlay-jump-prev ()
    (interactive)
    (symbol-overlay-jump-prev)
    (recenter))

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
   ("C-M-z" . iflipb-previous-buffer)
   ;; Evocative of "C-c i", bury-buffer.
   ("C-c M-i" . iflipb-kill-buffer))
  :config
  (validate-setq iflipb-ignore-buffers nil)
  ;; Monkeypatch to call `my-kill-buffer` instead of the asky default.
  (defun iflipb-kill-buffer ()
    "Same as `kill-buffer' but keep the iflipb buffer list state."
    (interactive)
    (call-interactively #'util/my-kill-buffer)
    (if (iflipb-first-iflipb-buffer-switch-command)
        (setq last-command 'util/my-kill-buffer)
      (if (< iflipb-current-buffer-index (length (iflipb-interesting-buffers)))
          (iflipb-select-buffer iflipb-current-buffer-index)
        (iflipb-select-buffer (1- iflipb-current-buffer-index)))
      (setq last-command 'iflipb-kill-buffer))))

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
  (desktop-save-mode 1)
  (validate-setq desktop-auto-save-timeout 60)
  (validate-setq desktop-restore-eager 0)
  (validate-setq desktop-path (list variable-files-dir))
  (validate-setq desktop-save t)
  (validate-setq desktop-load-locked-desktop t)
  (validate-setq desktop-dirname variable-files-dir)
  (validate-setq desktop-globals-to-save (append '(helm-kill-ring-map
                                                   (kill-ring . 50)
                                                   recentf-list
                                                   minibuffer-history
                                                   register-alist
                                                   file-name-history)
                                                 desktop-globals-to-save))
  (validate-setq desktop-restore-frames t)
  (validate-setq desktop-restore-in-current-display t)
  (validate-setq desktop-restore-forces-onscreen nil)
  ;; Force desktop reloading on startup even in TTY
  ;; From https://emacs.stackexchange.com/a/45829
  (validate-setq desktop-restore-forces-onscreen nil)
  (add-hook 'desktop-after-read-hook
            (lambda ()
              (frameset-restore
               desktop-saved-frameset
               :reuse-frames (eq desktop-restore-reuses-frames t)
               :cleanup-frames (not (eq desktop-restore-reuses-frames 'keep))
               :force-display desktop-restore-in-current-display
               :force-onscreen desktop-restore-forces-onscreen))))

(use-package align-cljlet :ensure t)
(use-package avy :ensure t)
(use-package beacon :ensure t
  :config
  (diminish 'beacon-mode)
  (validate-setq beacon-color "#8700d7"))
(use-package easy-kill :ensure t)
(use-package ffap :ensure t)
(use-package find-file-in-project :ensure t)
(use-package flymake-cursor :ensure t);;todo
(use-package markdown-mode+ :ensure t)
(use-package saveplace :ensure t)
(use-package undo-tree :ensure t
  :config (diminish 'undo-tree-mode)
  (validate-setq undo-tree-visualizer-timestamps t))
(use-package yasnippet :ensure t
  :defer t
  :config
  (use-package clojure-snippets :ensure t)
  (yas-global-mode 1)
  (diminish 'yas-minor-mode)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  ;; Inherit local/ in a given mode by referencing it in that mode's .yas-parents.
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/local")
  (yas-load-directory "~/.emacs.d/snippets"))

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
  :bind (("C-x C-k" . util/my-kill-buffer)
         ("<f12>"   . bury-buffer)
         ("C-c C-i" . bury-buffer))
  :init
  (add-to-list 'kill-buffer-query-functions 'util/do-not-kill-important-buffers))

(use-package company :ensure t
  :config
  (diminish 'company-mode)
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

(use-package git-gutter :ensure t
  :config
  (use-package git-gutter+ :ensure t
    :config
    (use-package git-commit :ensure t))
  (validate-setq git-gutter:modified-sign "▌")
  (validate-setq git-gutter:added-sign "▌")
  (validate-setq git-gutter:deleted-sign "▌")
  (git-gutter-mode))

(use-package gist
  :defer t
  :ensure t
  ;; :bind (("C-c g g l" . gist-list)
  ;;        ("C-c g g b" . gist-region-or-buffer))
  :config
  (use-package gh :ensure t :defer t)
  (validate-setq gist-view-gist t))

(use-package ialign
  :ensure t
  :bind (("C-c l" . ialign)))

(save-place-mode)
(validate-setq save-place-file (concat variable-files-dir ".emacs-places"))

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
  ("C-c >" . mc/mark-next-like-this-word)
  :init
  (validate-setq mc/always-run-for-all t)
  (validate-setq mc/list-file (concat variable-files-dir ".mc-lists.el"))
  (validate-setq mc/edit-lines-empty-lines 'ignore))

(use-package helm-swoop :ensure t
  :bind
  ("C-s" . helm-swoop-without-pre-input)
  :config
  (validate-setq helm-swoop-speed-or-color t))

(use-package ripgrep :ensure t)

(use-package xref :ensure t
  :config
  (defun xref-pop-marker-stack ()
    "Pop back to where \\[xref-find-definitions] was last invoked.
Kills the has-def-buffer buffer if different from the source buffer."
    (interactive)
    (let ((has-def-buffer (window-buffer)))
      (let ((ring xref--marker-ring))
        (when (ring-empty-p ring)
          (user-error "Marker stack is empty"))
        (let ((marker (ring-remove ring 0)))
          (switch-to-buffer (or (marker-buffer marker)
                                (user-error "The marked buffer has been deleted")))
          (goto-char (marker-position marker))
          (set-marker marker nil nil)
          ;; The goal of this function override: we previously jumped
          ;; to a definition from a jump-source buffer to some
          ;; `has-def-buffer`. If these are two different buffers,
          ;; kill the extraneous `has-def-buffer`.
          (when (not (equal (window-buffer)
                            has-def-buffer))
            (kill-buffer has-def-buffer)))))))

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
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]resources$")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]logs$")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]node_modules$"))

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
(defalias 'orig-tab-bar-tab-name-current (symbol-function 'tab-bar-tab-name-current))
(defun tab-bar-tab-name-current ()
  "Pad default display of tab name."
  (format " %s " (funcall 'orig-tab-bar-tab-name-current)))

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
    ;; Here we force one global "secondary" notes file by issuing the
    ;; universal argument to an interactive call.
    (let ((current-prefix-arg 4)
          (side-notes-secondary-file (expand-file-name "~/Dropbox/docs/notes/notes.org"))
          (side-notes-display-alist '((side . right) (window-width . 100))))
      (call-interactively 'side-notes-toggle-notes)))
  (global-set-key (kbd "H-[") 'side-notes-toggle-main-notes)
  (global-set-key (kbd "H-]") 'side-notes-toggle-notes))

(use-package origami :ensure t
  :config
  (global-origami-mode)
  (global-set-key (kbd "<backtab>") 'origami-toggle-node)
  (global-set-key (kbd "C-c <backtab>") 'origami-open-node-recursively))

(use-package subword
  :config
  (diminish 'subword-mode)
  (global-subword-mode 1))

(use-package treemacs :ensure t
  :bind
  ("H-n" . treemacs)
  :config
  (use-package treemacs-projectile :ensure t)
  (use-package treemacs-magit :ensure t)
  (treemacs-git-mode 'deferred)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (validate-setq treemacs-recenter-after-file-follow 'always)
  (treemacs-modify-theme "Default"
    :config
    (progn
      (treemacs-create-icon :icon "" :extensions (dir-closed) :fallback (propertize "‣ " 'face 'treemacs-term-node-face))
      (treemacs-create-icon :icon "" :extensions (dir-open)   :fallback (propertize "▾ " 'face 'treemacs-term-node-face))))
  (setq treemacs-indentation-string (propertize " │ " 'face 'magit-diff-context)
        treemacs-indentation 1)
  (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?))

;; Change character used to draw vertical border
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?┊))

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-should-follow-file t)
  (setq dired-sidebar-theme 'nerd)
  (setq dired-sidebar-theme 'ascii)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(use-package elfeed :ensure t
  :init
  (use-package elfeed-goodies :ensure t
    :config
    (elfeed-goodies/setup))
  :config
  (validate-setq elfeed-search-filter "@6-months-ago")
  (validate-setq elfeed-feeds
                 '("https://stackoverflow.com/feeds/user/8746216" "https://clojuredesign.club/index.xml" "http://ask.datomic.com/index.php/feed/qa.rss" "https://clojureverse.org/posts.rss" "https://clojureverse.org/latest.rss" "https://www.reddit.com/r/clojure.rss" "https://ask.clojure.org/index.php/feed/questions.rss")))
