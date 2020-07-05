;; Make startup faster by reducing the frequency of garbage
;; collection.
(setq gc-cons-threshold (* 50 1000 1000))


;; "Emergency mode" -- accommodate less-painful startup routine when
;; troubleshooting config problems featuring many restarts.
(defun byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))
(global-whitespace-mode -1)

(setq load-prefer-newer t)

(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package benchmark-init :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package gcmh :ensure t
  :init
  (gcmh-mode 1)
  :config
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 16 1024 1024)  ; 16mb
        ))

;; Unset file-name-handler-alist temporarily -- speeds up loading packages.
(defvar orig--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
;; Reset it after startup.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist orig--file-name-handler-alist)))

(defvar dotfiles-dir (file-name-directory
                      (or (buffer-file-name) load-file-name)))
(setq user-config (concat dotfiles-dir user-login-name ".el"))
(when (file-exists-p user-config)
  (load user-config))

;; Load theme here after loading custom stuff. Something in Emacs 28
;; causes startup issues where loading themes prior to code in main
;; use-package file causes a "Attempt to set a constant symbol: nil"
;; error that halts code loading.
(if (util/is-in-terminal)
    (load-theme 'daemian t)
  (progn (load-theme 'daemian-gui t)
         ;; Maximize borderless. Assumes `emacs-plus` compiled with
         ;;   --with-no-titlebar option.
         (setq ns-auto-hide-menu-bar t)
         (set-frame-position nil 0 0)
         (set-frame-size nil (display-pixel-width) (display-pixel-height) t)))

(setq debug-ignored-errors nil)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(message "Done initializing.")
