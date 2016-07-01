(setq dotfiles-dir (file-name-directory
		    (or (buffer-file-name) load-file-name)))
(setq variable-files-dir (concat dotfiles-dir "var/"))

;; MacOS X specific stuff
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(add-to-list 'custom-theme-load-path dotfiles-dir)
(defun is-in-terminal () (not (display-graphic-p)))
(if (is-in-terminal)
    (load-theme 'daemian t)
  (load-theme 'twilight-anti-bright t))



(setq desktop-path (list variable-files-dir))
(setq desktop-save t)
(setq desktop-load-locked-desktop t)
(setq desktop-dirname variable-files-dir)
(setq desktop-globals-to-save (append '((kill-ring . 50)
					recentf-list
                                        minibuffer-history)))
(defun desktop-file-modtime-reset ()
  "Reset `desktop-file-modtime' so the user is not bothered."
  (interactive)
  (run-with-timer 5 nil
                  (lambda ()
                    (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))
                    (desktop-save desktop-dirname))))

(add-hook 'find-file-hook 'desktop-file-modtime-reset)
(add-hook 'kill-emacs-hook 'desktop-file-modtime-reset)


(fset 'yes-or-no-p 'y-or-n-p)

(setq show-paren-mode t)
(setq show-paren-style 'mixed)

;; Disable visual/audio erroring.
(setq visible-bell t)
(setq redisplay-dont-pause t)
(setq ring-bell-function (lambda nil (message "")))

(setq color-theme-is-global t)

(setq sentence-end-double-space nil)

(setq-default indent-tabs-mode nil)

(setq shift-select-mode nil)

(setq mouse-yank-at-point t)

(setq whitespace-style '(face trailing lines-tail tabs))
(setq whitespace-line-column 80)

(setq diff-switches "-u")
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
					 try-expand-dabbrev-all-buffers
					 try-expand-dabbrev-from-kill
					 try-complete-file-name-partially
					 try-complete-file-name
					 try-expand-all-abbrevs
					 try-expand-list
					 try-expand-line
					 try-complete-lisp-symbol-partially
					 try-complete-lisp-symbol))

(setq tramp-default-method "ssh")
;; For tramp with sudo with my bash config, the prompt of which terminates
;; in a newline. Tell tramp how to detect the end of my prompt. This needs work.
(setq shell-prompt-pattern "
")

(setq scroll-preserve-screen-position t)

(setq bookmark-default-file (concat variable-files-dir ".emacs.bmk"))

(setq list-colors-sort (quote hsv))

(setq-default save-place t)
(setq save-place-file (concat variable-files-dir ".emacs-places"))

(setq split-height-threshold nil)
(setq split-width-threshold 200)

;; Remove older backup files.
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(setq backup-directory-alist `(("." . ,(concat variable-files-dir "backups"))))

;; Move auto-save and backup files elsewhere.
(defvar user-temporary-file-directory
  (setq temporary-file-directory (concat variable-files-dir ".autosaves/")))
(make-directory user-temporary-file-directory t)

(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
                (,tramp-file-name-regexp nil)))

(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

;; If you set set-mark-command-repeat-pop to non-nil, then immediately
;; after you type C-u C-<SPC>, you can type C-<SPC> instead of C-u
;; C-<SPC> to cycle through the mark ring.
(setq set-mark-command-repeat-pop t)

(setq ido-mode 'both)
(setq ido-ubiquitous-mode t)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-enable-prefix nil)
(setq ido-auto-merge-work-directories-length nil)
(setq ido-confirm-unique-completion t)
(setq ido-create-new-buffer 'always)
(setq ido-enter-matching-directory 'first)
(setq ido-max-prospects 10)
(setq ido-max-work-file-list 50)
(setq ido-use-filename-at-point 'guess)
(setq ido-use-virtual-buffers t)
(setq flx-ido-mode t)

(setq ido-save-directory-list-file (concat variable-files-dir "ido.last"))
;; Make ido-mode list things vertically
(setq ido-decorations
      (quote
       ("\n-> "           ; Opening bracket around prospect list
        ""                ; Closing bracket around prospect list
        "\n   "           ; separator between prospects
        "\n   ..."        ; appears at end of truncated list of prospects
        "["               ; opening bracket around common match string
        "]"               ; closing bracket around common match string
        " [No match]"     ; displayed when there is no match
        " [Matched]"      ; displayed if there is a single match
        " [Not readable]" ; current diretory is not readable
        " [Too big]"      ; directory too big
        " [Confirm]")))

;; Projectile shows full relative paths
(setq projectile-show-paths-function 'projectile-hashify-with-relative-paths)
(setq projectile-cache-file (concat variable-files-dir "projectile.cache"))
(setq projectile-known-projects-file (concat variable-files-dir "projectile-bookmarks.eld"))

;; Still needed?
;; Necessary due to bug in ruby-mode.
(setq ruby-indent-level 2)


(setq package-user-dir (concat dotfiles-dir "elpa"))
;; If unable to verify packages upon `package-install`.
(setq package-check-signature nil)


(setq ac-comphist-file (concat variable-files-dir "ac-comphist.dat"))

;; Disable linum for some modes.
(setq linum-disabled-modes-list
      '(cider-repl-mode grep-mode compilation-mode git-commit-mode help-mode))
(defun linum-on ()
  (unless (or (minibufferp) (member major-mode linum-disabled-modes-list))
    (linum-mode 1)))

(setq cider-popup-on-error t)
(setq cider-prompt-save-file-on-load nil)
(setq cider-repl-history-file "~/.lein/cider-repl-history")
(setq cider-repl-result-prefix ";; => ")
(setq cider-repl-use-clojure-font-lock t)
(setq cider-show-error-buffer nil)
(setq cider-font-lock-dynamically '(macro core function var))
(setq nrepl-buffer-name-separator "/")
;; Don't prompt and don't save
(setq cider-prompt-save-file-on-load nil)
;; Just save without prompting
(setq cider-prompt-save-file-on-load 'always-save)
(setq cider-font-lock-dynamically (quote (var macro)))
(setq cider-overlays-use-font-lock t)

(setq highlight-symbol-idle-delay 0.2)

(setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq avy-goto-char-style 'at)

(setq css-indent-offset 2)

(setq beacon-blink-delay 0.05)
(setq beacon-blink-duration 0.05)
(setq beacon-size 50)

(setq neo-window-width 45)
(setq neo-window-fixed-size nil)
(setq neo-smart-open t)
(setq neo-theme 'nerd)
(setq neo-vc-integration nil)

(setq magit-process-popup-time 10)
(setq magit-diff-refine-hunk 'all)

(setq rings-protect-buffers-in-rings nil)

(setq org-src-fontify-natively t)
(setq org-hide-leading-stars t)

;; At bottom to resolve some ordering issue.
(setq recentf-save-file (concat variable-files-dir ".recentf"))
