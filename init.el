;; Make startup faster by reducing the frequency of garbage
;; collection.
(setq gc-cons-threshold (* 50 1000 1000))

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

(put 'downcase-region 'disabled nil)

(defvar dotfiles-dir (file-name-directory
                      (or (buffer-file-name) load-file-name)))
(setq user-config (concat dotfiles-dir user-login-name ".el"))
(when (file-exists-p user-config)
  (load user-config))

(setq stack-trace-on-error '(buffer-read-only))

(setq debug-ignored-errors nil)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
(package-install 'use-package)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(zel yascroll yaml-mode which-key web-mode volatile-highlights validate use-package undo-tree typo treepy terraform-mode telephone-line symbol-overlay swoop sublimity smooth-scrolling smooth-scroll smex smart-mode-line slack rings rainbow-delimiters python-mode project-explorer pinentry paren-face paradox ov org-cliplink noflet neotree nav marshal markdown-mode+ magit lua-mode logview kv kpm-list kill-ring-search keyfreq key-chord ivy-prescient ivy-hydra inf-clojure iflipb ido-vertical-mode ido-ubiquitous ibuffer-vc hl-todo highlight-symbol helpful helm-projectile helm-open-github helm-descbinds google-translate google-this gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter gist ghub+ frame-local focus flymake-cursor flycheck-joker flycheck-clojure flx-ido find-file-in-project face-explorer easy-kill dumb-jump dockerfile-mode dired+ diff-hl dash-functional csv-mode counsel company clojure-snippets cljr-helm cider-eval-sexp-fu centered-cursor-mode cbm bug-hunter buffer-stack browse-kill-ring benchmark-init beacon back-button avy ascii anaphora align-cljlet ag adaptive-wrap)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'magit-edit-line-commit 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
