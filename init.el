
;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Load path etc.

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit"))
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit/feature-mode"))
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit/magit"))

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))

;; Sex.
(global-subword-mode 1)

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'dot-mode)

;; backport some functionality to Emacs 22 if needed
(require 'dominating-file)

;; Load up ELPA, the package manager

(require 'package)
(package-initialize)
(require 'starter-kit-elpa)

(load "elpa-to-submit/nxhtml/autostart")
;; Let's see if autoloading nxhtml this way makes it a bit nicer.
;; (load "elpa-to-submit/nxhtml/autostart")
(mapc (lambda (list)
        (mapc (lambda (pair)
                (if (or (eq (cdr pair)
                            'html-mode)
                        (eq (cdr pair)
                            'php-mode))
                    (setcdr pair (lambda ()
                                   (require 'nxhtml-mode "elpa-to-submit/nxhtml/autostart")
                                   (nxhtml-mumamo-mode)))))
              list))
      (list auto-mode-alist magic-mode-alist))

(add-to-list 'auto-mode-alist '("\\.org" . org-mode))
(add-to-list 'auto-mode-alist '("\\.css.*" . rainbow-mode))
(add-to-list 'auto-mode-alist '("\\.js.*" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.clj" . clojure-mode))
;; Load up starter kit customizations

(require 'starter-kit-defuns)
(require 'starter-kit-bindings)
(require 'starter-kit-misc)
(require 'starter-kit-registers)
(require 'starter-kit-eshell)
(require 'starter-kit-lisp)
;;(require 'starter-kit-perl)
(require 'starter-kit-ruby)
(require 'starter-kit-js)

(regen-autoloads)
(load custom-file 'noerror)

;; Disable auto-fill-mode.
(auto-fill-mode nil)

;; Work around a bug on OS X where system-name is FQDN
(if (eq system-type 'darwin)
    (setq system-name (car (split-string system-name "\\."))))

;; You can keep system- or user-specific customizations here
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-config) (load user-specific-config))
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))



;; My additions
(global-hl-line-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(setq bookmark-default-file (concat dotfiles-dir ".emacs.bmk"))

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(setq py-python-command-args '("-colors" "Linux"))
(setq ipython-command "/usr/bin/ipython")
(require 'ipython)

(require 'sqlplus)

;; For tramp with sudo.
(setq tramp-default-method "ssh")
;; On startup, don't check recent-file readability. If I've used tramp
;; and sudo: to edit a file, emacs init will hang otherwise.
(recentf-mode 1)
;; For tramp with sudo with my bash config, the prompt of which terminates
;; in a newline. Tell tramp how to detect the end of my prompt.
(setq shell-prompt-pattern "
")

;; Save point position when page-down- or page-uping.
(setq scroll-preserve-screen-position t)

;; Controls how many non-permanent entries are shown in the recent-files list. The default is 15. 
(setq recent-files-number-of-entries 100)
(setq recentf-max-menu-items 100)
;; Obvious.
(setq recentf-exclude (append recentf-exclude '(".ftp:.*" ".sudo:.*" "~$" "/.autosaves/")))

;; Let C-xk kill buffers as normal even when there's a client listening.
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; post-mode if coming in via sup.
(add-to-list 'auto-mode-alist '("sup\\.\\(compose\\|forward\\|reply\\|resume\\)-mode$" . post-mode))

;; Copy vim's "set scroll-off=10" setting.
;; (require 'smooth-scrolling)


;; Provide a python compile.
(defun my-compile ()
  "Use compile to run python programs"
    (interactive)
      (compile (concat "python " (buffer-file-name))))
(setq compilation-scroll-output t)
;; This should be in a python-mode hook function.
(local-set-key "\C-c\C-c" 'my-compile)

(defun my-kill-buffer ()
  "Just kill the current buffer without asking, unless of course it's a modified file"
  (interactive)
  (kill-buffer (current-buffer)))

;; Move auto-save and backup files elsewhere.
(defvar user-temporary-file-directory
  (setq temporary-file-directory "~/.emacs.d/.autosaves"))

(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
                (,tramp-file-name-regexp nil)))

(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))

(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))


;; highlight-symbol mode
(require 'highlight-symbol)

(defface highlight-symbol-face
  '((((class color) (background light))
     (:background "gray10"))
    (((class color) (background dark))
     (:background "gray90")))
  "*Face used by `highlight-symbol-mode'."
  :group 'highlight-symbol)

;; paren matching
(setq show-paren-delay 0)           ; how long to wait?
(show-paren-mode t)                 ; turn paren-mode on
(setq show-paren-style 'expression) ; alternatives are 'parenthesis' and 'mixed'

(set-face-background 'show-paren-match-face "#090909")
(set-face-foreground 'show-paren-match-face "#3e00f8")
(set-face-attribute 'show-paren-match-face nil 
        :weight 'normal :underline nil :overline nil :slant 'normal)

(set-face-foreground 'show-paren-mismatch-face "white")
(set-face-background 'show-paren-mismatch-face "red")
(set-face-attribute 'show-paren-mismatch-face nil 
                    :weight 'bold :underline nil :overline nil :slant 'normal);;; init.el ends here

(require 'nav)
(require 'python-mode)

;; mozilla-emacs key bindings

(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(add-hook 'javascript-mode-hook 'javascript-custom-setup)

(defun javascript-custom-setup ()
  (moz-minor-mode 1))

;;; MacOS X specific stuff
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)

(add-hook 'javascript-mode-hook 'javascript-custom-setup)
(defun javascript-custom-setup ()
  (moz-minor-mode 1))

(add-hook 'python-mode-hook 'moz-minor-mode)
(add-hook 'python-mode-hook 'flymake-mode)
(add-hook 'ruby-mode-hook 'flymake-mode)
(add-hook 'nxml-mode-hook 'moz-minor-mode)

(add-hook 'text-mode-hook 'turn-off-auto-fill)

;; bind CTRL-X P to Mozilla refresh browser
(global-set-key (kbd "C-x m")
                (lambda ()
                  (save-buffer)
                  (interactive)
                  (comint-send-string (inferior-moz-process)
                                      "BrowserReload();")))

;; zen-coding addon
(require 'zencoding-mode)
(add-hook 'nxml-mode-hook 'zencoding-mode)
(global-set-key (kbd "C-c [") 'zencoding-expand-line)
(global-set-key (kbd "C-c ]") 'zencoding-preview-accept)
(global-set-key (kbd "C-x C-\\") 'goto-last-change)
;; Restore a sane and non-eyeball-murdering background color for certain modes.
;; I want these in place but they're causing errors on launch. Figure out why.
;;(set-face-background mumamo-background-chunk-submode1 nil)
;;(set-face-background mumamo-background-chunk-major nil)
(require 'magit)
(set-face-background 'magit-item-highlight "color-233")


(require 'breadcrumb)

;; bookmark.el
(global-set-key [(control x) (j)]       'bc-set)            ;; Set bookmark.
(global-set-key [(meta j)]              'bc-previous)       ;; M-j for jump to previous
(global-set-key [(shift meta j)]        'bc-next)           ;; Shift-M-j for jump to next
(global-set-key [(meta up)]             'bc-local-previous) ;; M-up-arrow for local previous
(global-set-key [(meta down)]           'bc-local-next)     ;; M-down-arrow for local next
(global-set-key [(control c)(j)]        'bc-goto-current)   ;; C-c j for jump to current bookmark
(global-set-key [(control x)(meta j)]   'bc-list)           ;; C-x M-j for the bookmark menu list

;; tmux handles shift+arrow differently than screen. Accomodate. Prefer a fix in .tmux.conf.
(global-set-key (kbd "M-[ d") 'windmove-left)
(global-set-key (kbd "M-[ c") 'windmove-right)
(global-set-key (kbd "M-[ a") 'windmove-up)
(global-set-key (kbd "M-[ b") 'windmove-down)


;; When I'm working from an OSX keyboard, make the delete key work the
;; way I'm used to everywhere else. That is, delete backward, and
;; delete backward a word if I hold down Alt.
(defun darwinize ()
  (interactive)

  (global-set-key (kbd "C-M-d") (quote backward-kill-word))
  )

(require 'yasnippet)
(set-face-foreground 'yas/field-highlight-face "#ffffff")
(set-face-background 'yas/field-highlight-face "color-56")
(set-face-foreground 'yas/field-debug-face "#222222")

(setq yas/root-directory "~/.emacs.d/elpa-to-submit/snippets")
(yas/load-directory yas/root-directory)
;; To globally enable the minor mode in *all* buffers
(yas/global-mode)

;; Rebind quit key and make it harder to hit. I rarely use it on purpose.
(global-unset-key "\C-x\C-c")
(global-set-key "\C-x\M-c" 'save-buffers-kill-emacs)
;; I do, however, kill the hell out of some buffers. If I add a C- to the second keystroke, kill without confirmation.
(global-set-key "\C-x\C-k" 'my-kill-buffer)

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;; oddly this is now necessary directly on kimjung...
(global-set-key (kbd "C-M-d") (quote backward-kill-word))

;; when is this necessary?
;; from hackintosh to kimjun under screen/screen and term xterm-256color
;; global--key [(control d)]       'delete-char)            ;; s

;; Now that I do most of my work from an OSX keyboard, I don't need to
;; run this toggle at startup usually.
;; not needed: work hackintosh with mac keyboard on iterm
;; needed: home macbook (!?)
;;(normal-erase-is-backspace-mode)

;; Run shellhist, which preserves command history in eshell within
;; applications (e.g., python console or mysql prompt).
(require 'eshell)
(add-hook 'eshell-mode-hook 'shellhist-instrument-eshell)
(defun m-eshell-hook ()
; define control p, control n and the up/down arrow
(define-key eshell-mode-map [(control p)] 'eshell-previous-matching-input-from-input)
(define-key eshell-mode-map [(control n)] 'eshell-next-matching-input-from-input)

(define-key eshell-mode-map [up] 'eshell-previous-matching-input-from-input)
(define-key eshell-mode-map [down] 'eshell-next-matching-input-from-input)

(add-hook 'eshell-mode-hook 'm-eshell-hook))

;; Override keystroke for query-replace. I almost always want the -regexp version instead.
(global-set-key (kbd "M-%") 'query-replace-regexp)

(defun my-previous-10 ()
  (interactive)
  (previous-line) (recenter)
  (previous-line) (recenter)
  (previous-line) (recenter)
  (previous-line) (recenter)
  (previous-line) (recenter)
  (previous-line) (recenter)
  (previous-line) (recenter)
  (previous-line) (recenter)
  (previous-line) (recenter)
  (previous-line) (recenter))
(global-set-key (kbd "ESC <up>") 'my-previous-10)

(defun my-forward-10 ()
  (interactive)
  (forward-line) (recenter)
  (forward-line) (recenter)
  (forward-line) (recenter)
  (forward-line) (recenter)
  (forward-line) (recenter)
  (forward-line) (recenter)
  (forward-line) (recenter)
  (forward-line) (recenter)
  (forward-line) (recenter)
  (forward-line) (recenter))
(global-set-key (kbd "ESC <down>") 'my-forward-10)

(defun my-forward-paragraph ()
  (interactive)
  (forward-paragraph)
  (recenter))
(global-set-key (kbd "M-}") (quote my-forward-paragraph))

(defun my-backward-paragraph ()
  (interactive)
  (backward-paragraph)
  (recenter))
(global-set-key (kbd "M-{") (quote my-backward-paragraph))

(require 'buffer-stack)
(global-set-key (kbd "C-z") 'buffer-stack-down)
(global-set-key (kbd "M-C-z") 'buffer-stack-up)
(global-set-key [(f9)] 'buffer-stack-track)
(global-set-key [(control f9)] 'buffer-stack-untrack)
(global-set-key [(f12)] 'buffer-stack-bury)
(global-set-key [(control f12)] 'buffer-stack-bury-and-kill)


;; Built-in keybinding for dot-mode-execute (C-.) isn't detected for me.
(global-set-key (kbd "C-M-r") 'dot-mode-execute)


;; save a list of open files in ~/.emacs.desktop
;; save the desktop file automatically if it already exists
;; M-x desktop-save necessary to first create this file, auto-updates thereafter.
(setq desktop-save 'if-exists)
(desktop-save-mode 1)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                register-alist)))

;; Hopefully this fixes the bug where having a number of similarly-named buffers open
;; eventually results in being unable to switch to some of them ("you have selected a deleted buffer").
(defadvice iswitchb-kill-buffer (after rescan-after-kill activate)
  "*Regenerate the list of matching buffer names after a kill.
    Necessary if using niquify' with niquify-after-kill-buffer-p'
    set to non-nil."
  (setq iswitchb-buflist iswitchb-matches)
  (iswitchb-rescan))
(defun iswitchb-rescan ()
  "*Regenerate the list of matching buffer names."
  (interactive)
  (iswitchb-make-buflist iswitchb-default)
  (setq iswitchb-rescan t))

;; (require 'ibuffer-git)

;;; Fix junk characters in shell mode
(add-hook 'shell-mode-hook
          'ansi-color-for-comint-mode-on)


(require 'kill-ring-search)
(global-set-key "\M-\C-y" 'kill-ring-search)

;; (require 'sunrise-commander)
;; (global-set-key (kbd "C-c x") 'sunrise)
;; (add-to-list 'auto-mode-alist '("\\.srvm\\'" . sr-virtual-mode))

;; Let's not have to launch this stuff manually anymore.
(defun pg ()
  (interactive)
  (progn (setf pg-buffer (shell "*pg*")) (comint-send-string (get-buffer-process pg-buffer) "psql -U geo geodjango\n"))
  )
(defun repl ()
  (interactive)
  (progn (setf repl-buffer (shell "*repl*")) (comint-send-string (get-buffer-process repl-buffer) "source .bashrc && ip\n")))

(defun log ()
  (interactive) (progn (setf log-buffer (shell "*log*")) (comint-send-string (get-buffer-process log-buffer) "tail -f /var/log/apache2/error.log\ntail -f /var/log/cloud/job.log | grep --line-buffered -v DEBUG.*SQL.*NULL")))

(defun free ()
  (interactive)
  (progn (setf free-buffer (shell "*free*")) ))

(defun work-shells ()
  (interactive)
  (repl)
  (log)
  (free))

(defun mysql ()
  (interactive)
  (progn (setf mysql-buffer (shell "*mysql*")) (comint-send-string (get-buffer-process mysql-buffer) "mysql -u root -p geck\n"))
  )

(defun mailserver ()
  (interactive)
  (progn (setf mailserver-buffer (shell "*mailserver*")) (comint-send-string (get-buffer-process mailserver-buffer) "python -m smtpd -n -c DebuggingServer localhost:1025\n"))
  )

(defun django-shell ()
  (interactive)
  (progn (setf django-shell-buffer (shell "*django-shell*")) (comint-send-string (get-buffer-process django-shell-buffer) "cd ~/jangoes/geck; workon geck12; python manage.py shell_plus\n"))
  )

(defun free ()
  (interactive)
  (progn (setf free-shell-buffer (shell "*free*")))
  )

;; ...or even one-at-a-time.
(defun shells ()
  (interactive)
  (pg)
  (django-shell))

(defun geck-shells ()
  (interactive)
  (mailserver)
  (mysql)
  (django-shell))

(load-library "flymake-cursor")
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
;; Make sure it's not a remote buffer or flymake would not work
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list "pyflakes" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
                              '("\\.py\\'" flymake-pyflakes-init))) 

(defun my-isearch-word-at-point ()
  (interactive)
  (call-interactively 'isearch-forward-regexp))

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
(global-set-key "\M-\C-s" 'my-isearch-word-at-point)


(global-set-key (kbd "C-c ,") 'tags-search)

;; Let existence of an active region determine whether we backward-kill-word, or kill said region.
(defun backward-kill-word-or-kill-region (&optional arg)
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

(global-set-key (kbd "C-w") 'backward-kill-word-or-kill-region)
;;; init.el ends here
(put 'downcase-region 'disabled nil)

(put 'scroll-left 'disabled nil)

(setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8"))
;; (require 'evernote-mode)
;; (global-set-key (kbd "C-c ec") 'evernote-create-note)
;; (global-set-key (kbd "C-c eo") 'evernote-open-note)
;; (global-set-key (kbd "C-c es") 'evernote-search-notes)
;; (global-set-key (kbd "C-c eS") 'evernote-do-saved-search)
;; (global-set-key (kbd "C-c ew") 'evernote-write-note)
;; (global-set-key (kbd "C-c ep") 'evernote-post-region)
;; (global-set-key (kbd "C-c eb") 'evernote-browser)

(require 'clojure-mode)
(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)
(require 'slime)
;; (slime-setup)
;; (setq slime-use-autodoc-mode nil) ;; Workaround for Clojure 1.3. See http://groups.google.com/group/clojure/browse_thread/thread/692c3a93bbdf740c?tvc=2&pli=1

;; ;; paredit
;; (require 'paredit)
;; (require 'highlight-parentheses)
;; (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
;; (add-hook 'clojure-mode-hook
;;           (lambda ()
;;             (highlight-parentheses-mode t)
;;             (paredit-mode t)
;;             (slime-mode t)))
;; (setq hl-paren-colors
;;       '("red1" "orange1" "yellow1" "green1" "cyan1"
;;         "slateblue1" "magenta1" "purple"))

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c M-x") 'execute-extended-command)

(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

; Like Alt-m but to point after = sign.
(fset 'back-to-operand
   "\C-a\C-s=\C-m\C-[f\C-[b")
(global-set-key (kbd "C-c m") 'back-to-operand)

; Makes region-killing work in graphical emacs.
(defun region-active-p ()  (and transient-mark-mode mark-active))

;; clock in modeline
(display-time-mode 1)
(defface egoge-display-time
  '((((type x w32 mac))
     ;; #060525 is the background colour of my default face.
     (:foreground "#ffaa00" :inherit bold))
    (((type tty))
     (:foreground "color-130")))
  "Face used to display the time in the mode line.")
;; This causes the current time in the mode line to be displayed in
;; goge-display-time-face' to make it stand out visually.
(setq display-time-string-forms
      '((propertize (concat " " dayname " " monthname " " day ", " 12-hours ":" minutes " ")
                    'face 'egoge-display-time)))

(setq sentence-end-double-space nil) ;period single space ends sentence
;; Disable all window erroring.
(setq redisplay-dont-pause t)
(setq visible-bell nil)
(setq ring-bell-function(lambda nil (message "")))
(setq org-agenda-custom-commands
      '(("H" "Home Lists"
         ((agenda)
          (tags-todo "DONE")))
        
        ("D" "Daily Action List"
         ((agenda "" ((org-agenda-ndays 7)
                      (org-agenda-sorting-strategy
                       (quote ((agenda time-up priority-down tag-up) )))
                      (org-deadline-warning-days 0)
                      ))))
        )
      )
;; Make shift-up work.
(define-key input-decode-map "\e[1;2A" [S-up])
;; Unnecessary for shift-up at least from a Darwin console.
;; (if (equal "xterm" (tty-type))
;;     (define-key input-decode-map "\e[1;2A" [S-up]))
;; (defadvice terminal-init-xterm (after select-shift-up activate)
;;       (define-key input-decode-map "\e[1;2A" [S-up]))

(jiggle-mode 1)
(setq jiggle-how-many-times 2)
(setq jiggle-sit-for-how-long .01)

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

(setq-default
 header-line-format
 '(; Position, including warning for 80 columns
   (:propertize "%4l" face mode-line-position-face)
   ","
 (:eval (propertize "%1c" 'face
 (if (>= (current-column) 80)
 'mode-line-80col-face
 'mode-line-position-face)))
 ; Percentage of buffer above viewport.
 " %p "
 ; read-only or modified status
 (global-mode-string global-mode-string)))


(setq-default
 mode-line-format
 '(; Position, including warning for 80 columns
    " "
 (:eval
  (cond (buffer-read-only
         (propertize "RO " 'face 'mode-line-read-only-face))
        ((buffer-modified-p)
         (propertize " * " 'face 'mode-line-modified-face))
        (t "   ")))

   (:eval (propertize "%3c" 'face
                      'mode-line-80col-face
                      'mode-line-position-face))
                                        ; emacsclient [default -- keep?]
   mode-line-client
                                        ; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)
                                        ; narrow [default -- keep?]
   " %n "
                                        ; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (vc-mode vc-mode)
   " %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%] "
   (:eval (propertize (format-mode-line minor-mode-alist)
   'face 'mode-line-minor-mode-face)
          (:propertize mode-line-process
                       face mode-line-process-face)
" "
                                        ; nyan-mode uses nyan cat as an alternative to %p
(:eval (when nyan-mode (list (nyan-create))))
)))

;; (setq-default
;;  mode-line-format
;;  '(; Position, including warning for 80 columns
;;    (:propertize (:eval (shorten-directory default-directory 30))
;;                 face mode-line-folder-face)
;;    (:propertize "%b"
;;                 face mode-line-filename-face)
;;                                         ; narrow [default -- keep?]
;;    " %n "
;;                                         ; mode indicators: vc, recursive edit, major mode, minor modes,
;;                                         ; process, global
;;    (vc-mode vc-mode)
;;    " %["
;;    (:propertize mode-name
;;                 face mode-line-mode-face)
;;    "%] "
;;    (:eval (propertize (format-mode-line minor-mode-alist)
;;                       'face
;;                       'mode-line-minor-mode-face))
;;    (:propertize mode-line-process face mode-line-process-face)
;;    " "
;;                                         ; nyan-mode uses nyan cat as an alternative to %p
;;    (:eval (when nyan-mode (list (nyan-create))))
;;    ))



;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(set-face-attribute 'mode-line nil
    :foreground "blue" :background "gray20"
    :inverse-video nil)
(set-face-attribute 'mode-line-inactive nil
    :foreground "black" :background "gray20"
    :inverse-video nil
    :box '(:line-width 6 :color "gray40" :style nil))

(set-face-attribute 'mode-line-read-only-face nil
    :inherit 'mode-line-face
    :foreground "#4271ae"
    :box '(:line-width 2 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil
    :inherit 'mode-line-face
    :foreground "#ffffff"
    :background "#c82829"
    :box '(:line-width 2 :color "#ffffff"))
(set-face-attribute 'mode-line-folder-face nil
    :inherit 'mode-line-face
    :foreground "gray60")
(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground "#eab700"
    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
    :inherit 'mode-line-face
    :family "Menlo" :height 100)
(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face
    :foreground "gray80")
(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground "gray40"
    :height 110)
(set-face-attribute 'mode-line-process-face nil
    :inherit 'mode-line-face
    :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil
    :inherit 'mode-line-position-face
    :foreground "black" :background "#eab700")


;; Make shift-up work instead of triggering "<select> is undefined".
(defadvice terminal-init-xterm (after select-shift-up activate)
      (define-key input-decode-map "\e[1;2A" [S-up]))


;; If you set set-mark-command-repeat-pop to non-nil, then immediately
;; after you type C-u C-<SPC>, you can type C-<SPC> instead of C-u
;; C-<SPC> to cycle through the mark ring.
(setq set-mark-command-repeat-pop t)
