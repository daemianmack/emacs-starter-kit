
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


(setq dotfiles-dir (file-name-directory
                     (or (buffer-file-name) load-file-name)))


(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "elpa"))
(add-to-list 'load-path (concat dotfiles-dir "elpa-to-submit"))
(add-to-list 'load-path (concat dotfiles-dir "elpa-to-submit/feature-mode"))
(add-to-list 'load-path (concat dotfiles-dir "elpa-to-submit/magit"))
(add-to-list 'load-path (concat dotfiles-dir "elpa-to-submit/mark-multiple"))
(add-to-list 'load-path (concat dotfiles-dir "elpa-to-submit/expand-region"))


(let ((default-directory (concat dotfiles-dir "elpa")))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (normal-top-level-add-subdirs-to-load-path))
         load-path)))

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))

(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

;; Treat camelCasedWords as camel_cased_words.
(global-subword-mode 1)

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session
(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)

(add-to-list 'auto-mode-alist '("\\.org" . org-mode))
(add-to-list 'auto-mode-alist '("\\.css\\.*" . rainbow-mode))
(add-to-list 'auto-mode-alist '("\\.js\\.*" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.clj" . clojure-mode))

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
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))

(fset 'yes-or-no-p 'y-or-n-p)

(setq bookmark-default-file (concat dotfiles-dir ".emacs.bmk"))
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(require 'ipython)
(setq py-python-command-args '("--colors=NoColor"))
(setq ipython-command "/usr/bin/ipython")

;; For tramp with sudo.
(setq tramp-default-method "ssh")
;; On startup, don't check recent-file readability. If I've used tramp
;; and sudo: to edit a file, emacs init will hang otherwise.

;; For tramp with sudo with my bash config, the prompt of which terminates
;; in a newline. Tell tramp how to detect the end of my prompt.
(setq shell-prompt-pattern "
")

;; Save point position when page-down- or page-uping.
(setq scroll-preserve-screen-position t)

;; Let C-xk kill buffers as normal even when there's a client listening.
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; Copy vim's "set scroll-off=10" setting.
(require 'smooth-scrolling)


;; Provide a python compile.
(defun my-compile ()
  "Use compile to run python programs"
    (interactive)
      (compile (concat "python " (buffer-file-name))))
(setq compilation-scroll-output t)
;; This should be in a python-mode hook function.
(local-set-key "\C-c\C-c" 'my-compile)

(defun my-kill-buffer ()
  "Just kill the current buffer without asking, unless it's a modified file"
  (interactive)
  (kill-buffer (current-buffer)))

;; Move auto-save and backup files elsewhere.
(defvar user-temporary-file-directory
  (setq temporary-file-directory (concat dotfiles-dir ".autosaves")))

(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
                (,tramp-file-name-regexp nil)))

(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

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

;; Restore a sane and non-eyeball-murdering background color for certain modes.
;; I want these in place but they're causing errors on launch. Figure out why.
;;(set-face-background mumamo-background-chunk-submode1 nil)
;;(set-face-background mumamo-background-chunk-major nil)

(require 'magit)
(set-face-background 'magit-item-highlight "color-233")

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

(defun explain-different-quit-keys ()
  (interactive)
  (message "Use C-x M-c to quit instead! It's harder to hit by accident.")
  )

;; Rebind quit key and make it harder to hit. I rarely use it on purpose.
(global-set-key "\C-x\C-c" 'explain-different-quit-keys)
(global-set-key "\C-x\M-c" 'save-buffers-kill-emacs)
;; I do, however, kill the hell out of some buffers. If I add a C- to the second keystroke, kill without confirmation.
(global-set-key "\C-x\C-k" 'my-kill-buffer)

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
  (dotimes (number 10)
    (previous-line) (recenter)))
(global-set-key (kbd "ESC <up>") 'my-previous-10)

(defun my-forward-10 ()
  (interactive)
  (dotimes (number 10)
    (forward-line) (recenter)))
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

;; ;; Hopefully this fixes the bug where having a number of similarly-named buffers open
;; ;; eventually results in being unable to switch to some of them ("you have selected a deleted buffer").
;; (defadvice iswitchb-kill-buffer (after rescan-after-kill activate)
;;   "*Regenerate the list of matching buffer names after a kill.
;;     Necessary if using niquify' with niquify-after-kill-buffer-p'
;;     set to non-nil."
;;   (setq iswitchb-buflist iswitchb-matches)
;;   (iswitchb-rescan))
;; (defun iswitchb-rescan ()
;;   "*Regenerate the list of matching buffer names."
;;   (interactive)
;;   (iswitchb-make-buflist iswitchb-default)
;;   (setq iswitchb-rescan t))

;;; Fix junk characters in shell mode
(add-hook 'shell-mode-hook
          'ansi-color-for-comint-mode-on)


(require 'kill-ring-search)
(require 'browse-kill-ring)
(global-set-key "\M-\C-y" 'kill-ring-search)

(require 'sunrise-commander)
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

(require 'flymake-cursor)
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
;;(require 'slime)
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

;; (jiggle-mode 1)
;; (setq jiggle-how-many-times 2)
;; (setq jiggle-sit-for-how-long .01)

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
   (global-mode-string global-mode-string)
   "   "
   (:propertize (:eval which-func-current face mode-line-position-face)))
 )


(setq-default
 mode-line-format
 '((:eval
  (cond (buffer-read-only
         (propertize "  X  " 'face 'mode-line-read-only-face))
        ((buffer-modified-p)
         (propertize "  !  " 'face 'mode-line-modified-face))
        (t "   ")))
   ; emacsclient [default -- keep?]
   mode-line-client
   ; directory and buffer/file name
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

(set-face-attribute 'mode-line-inactive nil
    :foreground "black" :background "gray20")
(set-face-attribute 'mode-line-read-only-face nil
    :inherit 'mode-line-face
    :foreground "#000000"
    :background "blue"
    :box '(:line-width 2 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil
    :inherit 'mode-line-face
    :foreground "#ffffff"
    :background "#c82829"
    :weight 'bold
    :box '(:line-width 2 :color "#ffffff"))
(set-face-attribute 'mode-line-folder-face nil
    :inherit 'mode-line-face
    :foreground "black")
(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground "white")
(set-face-attribute 'mode-line-position-face nil
                    :inherit 'mode-line-face)
(set-face-attribute 'mode-line-mode-face nil
                    :inherit 'mode-line-face
    :foreground "gray80")
(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground "color-88"
    :height 110)
(set-face-attribute 'mode-line-process-face nil
    :inherit 'mode-line-face
    :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil
    :inherit 'mode-line-position-face
    :foreground "#000000" :background "yellow" :weight 'bold)


;; Make shift-up work instead of triggering "<select> is undefined".
(defadvice terminal-init-xterm (after select-shift-up activate)
      (define-key input-decode-map "\e[1;2A" [S-up]))


;; If you set set-mark-command-repeat-pop to non-nil, then immediately
;; after you type C-u C-<SPC>, you can type C-<SPC> instead of C-u
;; C-<SPC> to cycle through the mark ring.
(setq set-mark-command-repeat-pop t)


(defun vc-hg-annotate-command (file buffer &optional revision)
  "Execute \"hg annotate\" on FILE, inserting the contents in BUFFER.
Optional arg REVISION is a revision to annotate from."
  (vc-hg-command buffer 0 file "annotate" "-l" "-u" "-v" "-n" "-c"
                 (when revision (concat "-r" revision))))


(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
  (message "Ah, much better!"))

;; Saveplace
(setq-default save-place t)
;; Keep places in the load path
(setq save-place-file (concat dotfiles-dir ".emacs-places"))

(setq uniquify-separator ":")

;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

;; vi-style %
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(global-set-key "\M-%" 'goto-match-paren)


;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(global-set-key (kbd "C-x g") 'magit-status)

(require 'desktop)
(desktop-read)
;; full screen magit-status

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(require 'hlinum)