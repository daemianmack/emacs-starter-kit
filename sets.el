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
  (progn (load-theme 'daemian-gui t)
         ;; Maximize borderless. Assumes `emacs-plus` compiled with
         ;;   --with-no-titlebar option.
         (setq ns-auto-hide-menu-bar t)
         (set-frame-position nil 0 0)
         (set-frame-size nil (display-pixel-width) (display-pixel-height) t)))


(add-to-list 'completion-styles 'flex)

(setq desktop-path (list variable-files-dir))
(setq desktop-save t)
(setq desktop-load-locked-desktop t)
(setq desktop-dirname variable-files-dir)
(setq desktop-globals-to-save (append '(helm-kill-ring-map
                                        (kill-ring . 50)
                                        recentf-list
                                        minibuffer-history
                                        register-alist
                                        file-name-history)
					desktop-globals-to-save))
;; Force desktop reloading on startup even in TTY
;; From https://emacs.stackexchange.com/a/45829
(setq desktop-restore-forces-onscreen nil)
(add-hook 'desktop-after-read-hook
          (lambda ()
            (frameset-restore
             desktop-saved-frameset
             :reuse-frames (eq desktop-restore-reuses-frames t)
             :cleanup-frames (not (eq desktop-restore-reuses-frames 'keep))
             :force-display desktop-restore-in-current-display
             :force-onscreen desktop-restore-forces-onscreen)))


(fset 'yes-or-no-p 'y-or-n-p)

(setq show-paren-mode t)
(setq show-paren-style 'mixed)

;; Disable visual/audio erroring.
(setq visible-bell t)
(setq ring-bell-function (lambda nil (message "")))

(setq color-theme-is-global t)

(setq sentence-end-double-space nil)

(setq-default indent-tabs-mode nil)

(setq shift-select-mode nil)

(setq mouse-yank-at-point t)

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

(setq bookmark-default-file (concat variable-files-dir ".emacs.bmk"))

(setq list-colors-sort (quote hsv))

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

(setq package-user-dir (concat dotfiles-dir "elpa"))
;; If unable to verify packages upon `package-install`.
(setq package-check-signature nil)


(setq ac-comphist-file (concat variable-files-dir "ac-comphist.dat"))

(setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq avy-goto-char-style 'at)

(setq css-indent-offset 2)

(setq beacon-blink-delay 0.05)
(setq beacon-blink-duration 0.05)
(setq beacon-size 50)

(setq neo-window-width 45)
(setq neo-window-fixed-size t)
(setq neo-smart-open t)
(setq neo-theme 'arrow)
(setq neo-vc-integration nil)

(setq rings-protect-buffers-in-rings nil)
