
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
(add-to-list 'auto-mode-alist '("\\.js\\.*" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.clj" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.coffee\\.*" . coffee-mode))

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


(defun my-kill-buffer ()
  "Just kill the current buffer without asking, unless it's a modified file"
  (interactive)
  (kill-buffer (current-buffer)))

;; Move auto-save and backup files elsewhere.
(defvar user-temporary-file-directory
  (setq temporary-file-directory (concat dotfiles-dir ".autosaves/")))

(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
                (,tramp-file-name-regexp nil)))

(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

(require 'nav)

;;; MacOS X specific stuff
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)

(add-hook 'javascript-mode-hook 'javascript-custom-setup)
(defun javascript-custom-setup ()
  (moz-minor-mode 1))

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

;; ;; tmux handles shift+arrow differently than screen. Accomodate. Prefer a fix in .tmux.conf.
;; (global-set-key (kbd "M-[ d") 'windmove-left)
;; (global-set-key (kbd "M-[ c") 'windmove-right)
;; (global-set-key (kbd "M-[ a") 'windmove-up)
;; (global-set-key (kbd "M-[ b") 'windmove-down)

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

;;; Fix junk characters in shell mode
(add-hook 'shell-mode-hook
          'ansi-color-for-comint-mode-on)


(require 'kill-ring-search)
(require 'browse-kill-ring)
(global-set-key "\M-\C-y" 'kill-ring-search)

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

(require 'clojure-mode)
(defun turn-on-paredit ()
  (paredit-mode 1)
  ; TODO Does this need to be wrapped in a fn?
  (define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-round))
(add-hook 'clojure-mode-hook 'turn-on-paredit)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

(defun fix-coffeescript-tab-width () (setq tab-width 2))
(add-hook 'coffee-mode 'fix-coffeescript-tab-width)

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

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

(setq-default
 mode-line-format
 '(
   ; Position, including warning for 80 columns
   (:propertize "%4l" face mode-line-position-face)
   ","
   (:eval (propertize "%1c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   ; Percentage of buffer above viewport.
   " %p "
   ; directory and buffer/file name
   (:propertize "%b " face mode-line-filename-face)
   (:eval
    (cond (buffer-read-only
           (propertize "  X  " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize "  !  " 'face 'mode-line-modified-face))
          (t "     ")))
   ; emacsclient [default -- keep?]
   mode-line-client
   ; narrow [default -- keep?]
   " %n "
   ; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (vc-mode vc-mode)
   " %["
   (:propertize mode-name face mode-line-mode-face)
   "%] "
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face)
          (:propertize mode-line-process face mode-line-process-face)
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
    :foreground "bright blue" :background "color-233")
(set-face-attribute 'mode-line-read-only-face nil
    :inherit 'mode-line-face
    :foreground "#000000")
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
    :foreground "color-233")
(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground "bright black")
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
        " [Confirm]")))   ; confirm creation of new file or buffer

;; And let us use standard navagation keys that make sense vertically
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map [down] 'ido-next-match)
            (define-key ido-completion-map [up] 'ido-prev-match)
            (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
            (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)))

(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

;; vi-style %
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(global-set-key (kbd "C-c %") 'goto-match-paren)


;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(global-set-key (kbd "C-x g") 'magit-status)

(require 'desktop)
(desktop-read)

;; From http://blog.jenkster.com/2013/12/a-cider-excursion.html
;; Put [org.clojure/tools.namespace "0.2.4"] in ~/.lein/profiles.clj's
;; :user :dependencies vector
(defun cider-namespace-refresh ()
  (interactive)
  (cider-interactive-eval
   "(require 'clojure.tools.namespace.repl)
    (clojure.tools.namespace.repl/refresh)"))
(winner-mode 1)

(add-hook 'cider-mode-hook
          (lambda ()
            (define-key cider-mode-map (kbd "C-c M-r") 'cider-namespace-refresh)))

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


(require 'hlinum)


; *scratch* starts empty
(setq initial-scratch-message nil)

; Set *scratch* to Clojure mode
(when (locate-library "clojure-mode")
  (setq initial-major-mode 'clojure-mode))

(require 'projectile)
(projectile-global-mode)

; Projectile shows full relative paths
(setq projectile-show-paths-function 'projectile-hashify-with-relative-paths)

;; (require 'kibit-mode)
;; (add-hook 'clojure-mode-hook 'kibit-mode)
;; (add-hook 'clojure-mode-hook 'flymake-mode-on)

(defalias 'inf-ruby-keys 'inf-ruby-setup-keybindings)

(global-set-key (kbd "C-c C-g") 'rgrep)
(global-set-key (kbd "C-c ^") 'query-replace-regexp)

(require 'kpm-list)

(require 'ag)

;; Flips the left and right windows. Taken from
;; http://whattheemacsd.com//buffer-defuns.el-02.html
(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows) 1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2)))
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(global-set-key (kbd "C-x 4 r") 'rotate-windows)

;; I always hit space when I mean to jump. 
(global-set-key (kbd "C-x r a")   'point-to-register) ; "assign" to point
(global-set-key (kbd "C-x r SPC") 'jump-to-register)  ; easy jump target

; Necessary due to bug in ruby-mode.
(setq ruby-indent-level 2)

(require 'smart-forward)
(global-set-key (kbd "C-<up>") 'smart-up)
(global-set-key (kbd "C-<down>") 'smart-down)
(global-set-key (kbd "C-<left>") 'smart-backward)
(global-set-key (kbd "C-<right>") 'smart-forward)


(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (when server-buffer-clients
              (local-set-key (kbd "C-x C-k") 'server-edit))))


(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "jk" 'ace-jump-word-mode)
(key-chord-define-global "jl" 'other-window)
(key-chord-define-global "zz" 'save-buffer)
(key-chord-define-global "zx" 'beginning-of-buffer)
(key-chord-define-global "xc" 'end-of-buffer)

(require 'undo-tree)
(global-undo-tree-mode)

(defun is-in-terminal ()
  (not (display-graphic-p)))

(if (is-in-terminal)
    (load-theme 'daemian t)
  (load-theme 'twilight-anti-bright t))
(require 'rings)
(global-set-key (kbd "<f2>")   (rings-generate-cycler 2))
(global-set-key (kbd "C-c <f2>") (rings-generate-setter 2))
(global-set-key (kbd "<f3>")   (rings-generate-cycler 3))
(global-set-key (kbd "C-c <f3>") (rings-generate-setter 3))
(global-set-key (kbd "<f4>")   (rings-generate-cycler 4))
(global-set-key (kbd "C-c <f4>") (rings-generate-setter 4))
(global-set-key (kbd "<f5>")   (rings-generate-cycler 5))
(global-set-key (kbd "C-c <f5>") (rings-generate-setter 5))
(global-set-key (kbd "<f6>")   (rings-generate-cycler 6))
(global-set-key (kbd "C-c <f6>") (rings-generate-setter 6))

(setq split-height-threshold nil)
(setq split-width-threshold 200)

(require 'easy-kill)
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key (kbd "C-c =") 'easy-mark-sexp)

(defun reset-highlight ()
  (interactive)
  (global-hi-lock-mode 0)
  (global-hi-lock-mode 1))

(defun highlight-this ()
  (interactive)
  (highlight-regexp (regexp-quote (word-at-point))))

(global-unset-key (kbd "C-c e")) ; Don't need esk-eval-and-replace

(global-set-key (kbd "C-c e r") 'eval-region)

(global-set-key (kbd "C-c h t") 'highlight-this)

(require 'clojure-test-mode)

(defun windmove-emacs-or-tmux (dir tmux-cmd)
  (interactive)
  (if (ignore-errors (funcall (intern (concat "windmove-" dir))))
      nil                     ;; Moving within emacs
    (shell-command tmux-cmd)) ;; At edges, send command to tmux
  )

; Integrate with tmux splits.
(global-set-key (kbd "S-<up>")
                '(lambda () (interactive) (windmove-emacs-or-tmux "up"  "tmux select-pane -U")))
(global-set-key (kbd "S-<down>")
                '(lambda () (interactive) (windmove-emacs-or-tmux "down"  "tmux select-pane -D")))
(global-set-key (kbd "S-<right>")
                '(lambda () (interactive) (windmove-emacs-or-tmux "right" "tmux select-pane -R")))
(global-set-key (kbd "S-<left>")
                '(lambda () (interactive) (windmove-emacs-or-tmux "left"  "tmux select-pane -L")))

(defun write-region-to-tmux-buffer (beg end)
  (interactive "r")
  (shell-command-on-region beg end "tmux load-buffer -" nil nil nil t))

(defun write-buffer-to-tmux-buffer ()
  (interactive)
    (write-region-to-tmux-buffer (point-min) (point-max)))

(add-to-list 'auto-mode-alist '("\\.boot" . clojure-mode))

(require 'clj-refactor)
(cljr-add-keybindings-with-prefix "C-c C-f")

(defvar former-window-configuration nil
  "Stores previous window configurations, e.g. those that were in effect when center-window-horizontally was called.")


(defun center-window-horizontally (width)
  "Arrange windows three as side-by-side, with the center one
having width WIDTH.
Accepts WIDTH as a numeric prefix, but defaults to 85."
  (interactive "P")
  (push (current-window-configuration) former-window-configuration)
  (let ((width (or width 85)))
    (let ((side-window-width (/ (- (frame-parameter nil 'width) width) 2)))
      (delete-other-windows)
      (set-window-buffer (split-window-horizontally side-window-width)
                         (other-buffer nil nil))
      (other-window 1)
      (set-window-buffer (split-window-horizontally (- side-window-width))
                         (other-buffer nil nil)))))

(require 'project-explorer)

;; (defun pt-pbpaste ()
;;   "Paste data from pasteboard."
;;   (interactive)
;;   (shell-command-on-region
;;    (point)
;;    (if mark-active (mark) (point))
;;    "pbpaste" nil t))

;; (defun pt-pbcopy ()
;;   "Copy region to pasteboard."
;;   (interactive)
;;   (print (mark))
;;   (when mark-active
;;     (shell-command-on-region
;;      (point) (mark) "pbcopy")
;;     (kill-buffer "*Shell Command Output*")))

;; (global-set-key (kbd "C-x C-y") 'pt-pbpaste)
;; (global-set-key (kbd "C-x M-w") 'pt-pbcopy)

(setq interprogram-cut-function
      (lambda (text &optional push)
        (let* ((process-connection-type nil)
               (pbproxy (start-process "pbcopy" "pbcopy" "/usr/bin/pbcopy")))
          (process-send-string pbproxy text)
          (process-send-eof pbproxy))))

(setq interprogram-paste-function
      (lambda ()
                (shell-command-to-string "pbpaste")))
