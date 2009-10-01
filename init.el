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
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit/jabber"))

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

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

;; Load up starter kit customizations

(require 'starter-kit-defuns)
(require 'starter-kit-bindings)
(require 'starter-kit-misc)
(require 'starter-kit-registers)
(require 'starter-kit-eshell)
(require 'starter-kit-lisp)
(require 'starter-kit-perl)
(require 'starter-kit-ruby)
(require 'starter-kit-js)

(regen-autoloads)
(load custom-file 'noerror)

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
(fset 'yes-or-no-p 'y-or-n-p)

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


;; Start server.
(server-start)
;; Let C-xk kill buffers as normal even when there's a client listening.
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)


;; Copy vim's "set scroll-off=10" setting.
;; (require 'smooth-scrolling)


;; Provide a python compile.
(defun my-compile ()
  "Use compile to run python programs"
    (interactive)
      (compile (concat "python " (buffer-name))))
(setq compilation-scroll-output t)
;; This should be in a python-mode hook function.
(local-set-key "\C-c\C-c" 'my-compile)


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


(require 'nav)
(require 'python-mode)

;; mozilla-emacs key bindings

(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(add-hook 'javascript-mode-hook 'javascript-custom-setup)

(defun javascript-custom-setup ()
  (moz-minor-mode 1))

(add-hook 'javascript-mode-hook 'javascript-custom-setup)
(defun javascript-custom-setup ()
  (moz-minor-mode 1))

(add-hook 'python-mode-hook 'moz-minor-mode)
(add-hook 'nxml-mode-hook 'moz-minor-mode)


;; bind CTRL-X P to Mozilla refresh browser
(global-set-key (kbd "C-x p")
                (lambda ()
                  (save-buffer)
                  (interactive)
                  (comint-send-string (inferior-moz-process)
                                      "BrowserReload();")))

;; Restore a sane and non-eyeball-murdering background color for
;; certain modes.
(set-face-background mumamo-background-chunk-submode1 nil)

;;; init.el ends here
