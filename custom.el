(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-and-a-half-executable "/usr/local/bin/ack")
 '(backup-by-copying t)
 '(blink-cursor-alist nil)
 '(blink-cursor-mode t)
 '(buffer-stack-untracked (quote ("KILL" "*Compile-Log*" "*Ibuffer*" "*Compile-Log-Show*" "*Group*" "*Completions*" "*Kill Ring*" "*Messages*" "*nav*")))
 '(column-number-mode t)
 '(custom-safe-themes (quote ("e9a1226ffed627ec58294d77c62aa9561ec5f42309a1f7a2423c6227e34e3581" "951e10f17de57de1e0c9cbeb44fcdda1b6c6d26beab40c3bd0abbfc38dd5c9c8" "bf7ed640479049f1d74319ed004a9821072c1d9331bc1147e01d22748c18ebdf" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(desktop-after-read-hook (quote (list-buffers)))
 '(desktop-base-file-name "emacs.desktop")
 '(desktop-base-lock-name "lock")
 '(desktop-dir-name (quote ("~/.emacs.d")))
 '(desktop-files-not-to-save "^$")
 '(desktop-globals-to-clear (quote (regexp-search-ring regexp-search-ring-yank-pointer)))
 '(desktop-globals-to-save (quote ((extended-command-history . 30) (file-name-history . 100) (grep-history . 30) (compile-history . 30) (minibuffer-history . 50) (query-replace-history . 60) (read-expression-history . 60) (regexp-history . 60) (regexp-search-ring . 20) (search-ring . 20) (shell-command-history . 50) tags-file-name register-alist)))
 '(desktop-load-locked-desktop nil)
 '(desktop-path (quote ("~/.emacs.d")))
 '(desktop-save t)
 '(desktop-save-mode t)
 '(display-time-day-and-date t)
 '(display-time-mail-face nil)
 '(global-highlight-parentheses-mode t)
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(grep-highlight-matches t)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(hl-line-face (quote hl-line))
 '(hl-paren-background-colors (quote ("color-57")))
 '(hl-paren-colors (quote ("brightwhite" "IndianRed1" "IndianRed3" "IndianRed4")))
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(large-file-warning-threshold nil)
 '(linum-format "%d")
 '(list-matching-lines-face (quote default))
 '(magit-commit-all-when-nothing-staged nil)
 '(magit-process-popup-time 10)
 '(mode-line-in-non-selected-windows t)
 '(mumamo-chunk-coloring 0)
 '(nav-bookmark-list (quote ("~/src/git/cloud-quote" "~/.emacs.d" "/tmp" "/")))
 '(nav-default-width 30)
 '(nav-follow t)
 '(nav-quickdir-list (quote ("~/src/git/cloud-quote" "~/src/git/cloud-ui" "~/.emacs.d")))
 '(nav-quickfile-list (quote ("~/dotfiles/.bashrc" "" "")))
 '(nav-quickjump-show nil)
 '(nav-widths-percentile 99)
 '(org-agenda-custom-commands (quote (("x" "STUFF!" agenda "" nil))) t)
 '(org-agenda-files (quote ("~/Desktop/notes.org")))
 '(org-agenda-ndays 14)
 '(org-cycle-global-at-bob t)
 '(org-hide-leading-stars t)
 '(org-odd-levels-only nil)
 '(org-special-ctrl-a/e (quote reversed))
 '(package-user-dir "~/.emacs.d/elpa")
 '(recentf-max-saved-items 100)
 '(recentf-save-file "~/.emacs.d/.recentf")
 '(safe-local-variable-values (quote ((eval when (fboundp (quote rainbow-mode)) (rainbow-mode 1)) (whitespace-line-column . 80) (lexical-binding . t))))
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(show-trailing-whitespace t)
 '(size-indication-mode t)
 '(sqlplus-html-output-header "Sat Nov 24 10:54:11 2012<br><br>")
 '(sr-virtual-listing-switches "-aldp")
 '(uniquify-ask-about-buffer-names-p nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(uniquify-ignore-buffers-re nil)
 '(uniquify-min-dir-content 3)
 '(uniquify-separator nil)
 '(uniquify-trailing-separator-p nil)
 '(which-function-mode t)
 '(windmove-wrap-around t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-highlight-prompt ((t (:foreground "brightblue"))))
 '(cursor ((t (:background "selectedControlColor"))))
 '(cycbuf-current-face ((t (:background "color-52" :weight bold))) t)
 '(ediff-current-diff-B ((((class color) (min-colors 16)) (:background "Yellow" :foreground "black"))))
 '(ediff-even-diff-A ((((class color) (min-colors 16)) (:foreground "brightblue"))))
 '(ediff-even-diff-Ancestor ((((class color) (min-colors 16)) (:background "brightblue" :foreground "White"))))
 '(ediff-even-diff-B ((((class color) (min-colors 16)) (:foreground "brightblue"))))
 '(ediff-fine-diff-Ancestor ((((class color) (min-colors 16)) (:background "brightblack" :foreground "white"))))
 '(ediff-odd-diff-A ((((class color) (min-colors 16)) (:background "brightblack" :foreground "white"))))
 '(egoge-display-time ((((type tty)) (:background "black" :foreground "color-130"))))
 '(header-line ((t (:background "gray20" :weight bold))))
 '(highlight ((t (:background "bright blue" :foreground "white"))))
 '(highlight-symbol-face ((t (:background "red"))))
 '(hl-line ((t (:background "color-233"))))
 '(hl-paren-face ((t (:underline "red"))) t)
 '(idle-highlight ((t (:background "bright blue"))))
 '(isearch ((t (:background "magenta3" :foreground "lightskyblue1" :weight ultra-bold))))
 '(isearch-fail ((t (:background "red" :foreground "white" :weight ultra-bold))))
 '(italic ((((supports :underline t)) (:underline nil))))
 '(lazy-highlight ((t (:background "purple" :foreground "black" :weight ultra-bold))))
 '(magit-diff-file-header ((t (:background "green"))))
 '(magit-diff-hunk-header ((t (:inherit magit-header :background "blue" :foreground "white" :slant italic))))
 '(magit-diff-none ((t (:foreground "color-235"))))
 '(magit-header ((t (:foreground "color-45"))))
 '(minibuffer-prompt ((t (:foreground "#4271ae"))))
 '(mode-line ((t (:background "#4271ae" :foreground "#eeeeec" :weight bold))))
 '(mode-line-80col-face ((t (:inherit mode-line-position-face :background "yellow" :foreground "#000000"))) t)
 '(mode-line-emphasis ((t nil)))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:foreground "brightgreen" :box (:line-width 1 :color "grey40" :style released-button)))))
 '(mode-line-inactive ((t (:inherit mode-line :background "gray20" :weight bold))))
 '(mumamo-background-chunk-major ((t nil)))
 '(mumamo-background-chunk-submode2 ((((class color) (min-colors 88) (background light)) nil)))
 '(mumamo-background-chunk-submode3 ((((class color) (min-colors 88) (background light)) nil)))
 '(my-hl-line ((t (:background "red"))))
 '(py-XXX-tag-face ((t (:foreground "Firebrick"))) t)
 '(region ((t (:background "#4271ae" :foreground "black")))))
