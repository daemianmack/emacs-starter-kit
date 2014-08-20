(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-and-a-half-executable "/usr/local/bin/ack")
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#000000" "#ff0000" "#00ff00" "#ffff00" "#0000ff" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(ansi-term-color-vector
   ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(background-color "#ffffd7")
 '(background-mode light)
 '(backup-by-copying t)
 '(bc-bookmark-file (concat dotfiles-dir ".breadcrumb"))
 '(blink-cursor-alist nil)
 '(blink-cursor-mode t)
 '(blink-matching-delay 0.5)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/.emacs.bmk")
 '(buffer-stack-untracked
   (quote
    ("KILL" "*Compile-Log*" "*Ibuffer*" "*Compile-Log-Show*" "*Group*" "*Completions*" "*Kill Ring*" "*Messages*" "*nav*")))
 '(cider-repl-use-clojure-font-lock t)
 '(column-number-mode t)
 '(cursor-color "#626262")
 '(custom-safe-themes
   (quote
    ("5d98db0ee0af3681d987fc0a9ca3d32632cf18b73c50238fe9ba5a58ed797d93" "8ac31e1bc1920b33d478dfafb0b45989a00ede15a2388ea16093e7d0988c48d0" "01680272822bbb2d02f85f4fefb0684ab15bbd5b229ddfd4929ddececcec6d6c" "548f6cea041842d145ee35b90f1a07a74a51b14f77f1e931138f960c42ca7b39" "bfc0f14c5d5902b5154d57c6d3d14f8389b74997708aae3e98315795a0f0ca6b" "4a4475800ef69d5b3b6e02cd88a3fef391b2fb9bce5c7c9bfcb065cfa6ebce33" "e35ac8101749aa8ac01b588418a134d9ad42a74d5a1619228fe5fd4ac5f44898" "aaed3c12b6121161c0e3d142c6e25002e062fdfc47da26b5fd73c1f3b3eae0e0" "3223b32123d0890c8aa1674db7b6d84f0d31b70c7411a9ffd455ebfbcb3a5a82" "7400b80c7478b262314f9176cd616fe19b4457b67156145b4977a4b260216a32" "181bf8c5f71bb208ecd6a632fa6fa8cd86060dfbdd8fa82d5eae7df0c7c7b90e" "c2a2175e9ae03e328c1877f4b60bb0c1f78f796a2f433edf700acb41f7b1fec2" "34a0f577803a27674e5ae6bb2d82394dafd5252d9df751868b3821e02d0c5755" "ac24ba4e5adddd53aee6d4fd029a77ebf09920d59418e7a70d8a41ce7b78f9d6" "acca6c7438c8cc67b6b45da2841ad3452e2d9ab05b24eb8bafd2cd0985cf8ddc" "8340250882a74e751421b85cd18f277ab25c95fd63daa800ecbd42d79fc74f0f" "159bb8f86836ea30261ece64ac695dc490e871d57107016c09f286146f0dae64" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "e9a1226ffed627ec58294d77c62aa9561ec5f42309a1f7a2423c6227e34e3581" "951e10f17de57de1e0c9cbeb44fcdda1b6c6d26beab40c3bd0abbfc38dd5c9c8" "bf7ed640479049f1d74319ed004a9821072c1d9331bc1147e01d22748c18ebdf" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(desktop-after-read-hook (quote (list-buffers)))
 '(desktop-base-file-name "emacs.desktop")
 '(desktop-base-lock-name "lock")
 '(desktop-dirname nil t)
 '(desktop-files-not-to-save "^$")
 '(desktop-globals-to-clear
   (quote
    (regexp-search-ring regexp-search-ring-yank-pointer)))
 '(desktop-globals-to-save
   (quote
    ((extended-command-history . 30)
     (file-name-history . 100)
     (grep-history . 30)
     (compile-history . 30)
     (minibuffer-history . 50)
     (query-replace-history . 60)
     (read-expres\
      sion-history . 60)
     (regexp-history . 60)
     (regexp-search-ring . 20)
     (search-ring . 20)
     (shell-command-history . 50)
     (kill-ring . 20)
     tags-file-name register-alist)))
 '(desktop-load-locked-desktop nil)
 '(desktop-path (quote ("/Users/daemian/.emacs.d/")))
 '(desktop-save (quote if-exists))
 '(desktop-save-mode t)
 '(diff-switches "-u")
 '(display-time-day-and-date t)
 '(display-time-mail-face nil)
 '(fci-rule-character-color "#192028")
 '(fci-rule-color "#383838")
 '(foreground-color "#626262")
 '(global-highlight-parentheses-mode t)
 '(global-linum-mode t)
 '(global-rainbow-delimiters-mode t)
 '(grep-highlight-matches t)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(hippie-expand-try-functions-list
   (quote
    (try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name try-expand-all-abbrevs)))
 '(hl-paren-background-colors (quote ("color-57")))
 '(hl-paren-colors
   (quote
    ("brightwhite" "IndianRed1" "IndianRed3" "IndianRed4")))
 '(idle-highlight-idle-time 1.0)
 '(ido-auto-merge-work-directories-length nil)
 '(ido-confirm-unique-completion t)
 '(ido-create-new-buffer (quote always))
 '(ido-enable-flex-matching t)
 '(ido-enable-prefix nil)
 '(ido-enter-matching-directory (quote first))
 '(ido-everywhere t)
 '(ido-max-prospects 10)
 '(ido-max-work-file-list 50)
 '(ido-mode (quote both) nil (ido))
 '(ido-save-directory-list-file "~/.emacs.d/.ido.last")
 '(ido-ubiquitous-mode t)
 '(ido-use-filename-at-point (quote guess))
 '(ido-use-virtual-buffers t)
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(large-file-warning-threshold nil)
 '(linum-format "%d")
 '(list-matching-lines-face (quote default))
 '(magit-commit-all-when-nothing-staged nil)
 '(magit-diff-refine-hunk (quote all))
 '(magit-process-popup-time 3)
 '(mode-line-in-non-selected-windows t)
 '(mumamo-chunk-coloring 0)
 '(nav-default-width 30)
 '(nav-follow t)
 '(nav-quickjump-show nil)
 '(nav-width 54)
 '(nav-widths-percentile 99)
 '(org-agenda-ndays 14)
 '(org-cycle-global-at-bob t)
 '(org-hide-leading-stars t)
 '(org-level-color-stars-only nil)
 '(org-odd-levels-only t)
 '(org-replace-disputed-keys t)
 '(org-special-ctrl-a/e (quote reversed))
 '(package-user-dir (concat dotfiles-dir "elpa"))
 '(pe/side (quote right))
 '(projectile-ack-function (quote ack))
 '(recentf-max-saved-items 100)
 '(recentf-save-file (concat dotfiles-dir ".recentf"))
 '(safe-local-variable-values
   (quote
    ((eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1))
     (whitespace-line-column . 80)
     (lexical-binding . t))))
 '(same-window-regexps (quote ("\\*magit*")))
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
 '(vc-follow-symlinks t)
 '(which-function-mode t)
 '(windmove-wrap-around nil)
 '(window-sides-vertical t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-jump-face-background ((t (:foreground "color-236"))) t)
 '(ace-jump-face-foreground ((t (:background "purple" :foreground "white" :weight bold))) t)
 '(cider-error-highlight-face ((t (:inherit font-lock-warning-face :weight bold))))
 '(cider-repl-input-face ((t (:foreground "brightblack"))))
 '(cider-repl-output-face ((t nil)))
 '(cider-repl-prompt-face ((t (:background "black" :foreground "purple" :underline t))))
 '(cider-repl-result-face ((t (:foreground "blue"))))
 '(clojure-test-error-face ((t (:background "orange1" :foreground "black" :weight bold))))
 '(clojure-test-failure-face ((t (:background "orange red" :foreground "black" :weight bold))))
 '(clojure-test-success-face ((t (:foreground "green"))))
 '(comint-highlight-prompt ((t (:foreground "brightblue"))))
 '(cycbuf-current-face ((t (:background "color-52" :weight bold))) t)
 '(diff-added ((t (:foreground "green4"))))
 '(diff-added-face ((t (:foreground "green4"))) t)
 '(diff-changed ((t (:foreground "purple"))))
 '(diff-indicator-removed ((t (:inherit diff-removed))))
 '(diff-refine-added ((t (:inherit diff-refine-change))))
 '(diff-refine-change ((t (:foreground "purple"))))
 '(diff-refine-removed ((t (:inherit diff-refine-change))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red3"))))
 '(diff-removed-face ((t (:foreground "red3"))) t)
 '(ediff-current-diff-B ((((class color) (min-colors 16)) (:background "Yellow" :foreground "black"))) t)
 '(ediff-even-diff-A ((((class color) (min-colors 16)) (:foreground "brightblue"))) t)
 '(ediff-even-diff-Ancestor ((((class color) (min-colors 16)) (:background "brightblue" :foreground "White"))) t)
 '(ediff-even-diff-B ((((class color) (min-colors 16)) (:foreground "brightblue"))) t)
 '(ediff-fine-diff-Ancestor ((((class color) (min-colors 16)) (:background "brightblack" :foreground "white"))) t)
 '(ediff-odd-diff-A ((((class color) (min-colors 16)) (:background "brightblack" :foreground "white"))) t)
 '(egoge-display-time ((((type tty)) (:background "black" :foreground "color-130"))) t)
 '(font-lock-comment-face ((t (:inherit font-lock-string-face))))
 '(font-lock-function-name-face ((t (:foreground "bright Blue"))))
 '(header-line ((t (:background "color-234" :weight bold))))
 '(hi-black-b ((t (:foreground "black"))))
 '(hi-black-hb ((t (:background "black" :foreground "brightblack"))))
 '(hi-blue ((t (:background "blue1" :foreground "black"))))
 '(hi-green ((t (:background "green1" :foreground "black"))))
 '(hi-pink ((t (:background "color-165" :foreground "black"))))
 '(hi-yellow ((t (:background "yellow1" :foreground "black"))))
 '(highlight ((t (:background "bright blue" :foreground "white"))))
 '(highlight-symbol-face ((t (:background "red"))) t)
 '(hl-line ((t (:background "black"))) t)
 '(hl-paren-face ((t (:underline "red"))) t)
 '(idle-highlight ((t (:background "brightblack" :underline t))))
 '(italic ((((supports :underline t)) (:underline nil))))
 '(linum-highlight-face ((t (:inherit default :background "green" :foreground "black"))))
 '(magit-diff-file-header ((t (:background "black"))))
 '(magit-diff-hunk-header ((t (:background "blue" :foreground "white"))))
 '(magit-diff-none ((t (:foreground "color-235"))))
 '(magit-header ((t (:foreground "color-45"))))
 '(magit-item-highlight ((t nil)))
 '(magit-log-head-label-local ((t (:inherit magit-log-head-label-remote))))
 '(magit-log-head-label-remote ((t (:background "brightblue" :foreground "black"))))
 '(mode-line-80col-face ((t (:inherit mode-line-position-face :background "black" :foreground "yellow" :weight bold))) t)
 '(mode-line-emphasis ((t nil)))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:foreground "brightgreen" :box (:line-width 1 :color "grey40" :style released-button)))))
 '(mode-line-inactive ((t (:inherit mode-line :background "gray20" :weight bold))))
 '(modeline ((t (:background "#4271ae" :foreground "#eeeeec" :weight bold))) t)
 '(mumamo-background-chunk-major ((t nil)) t)
 '(mumamo-background-chunk-submode2 ((((class color) (min-colors 88) (background light)) nil)) t)
 '(mumamo-background-chunk-submode3 ((((class color) (min-colors 88) (background light)) nil)) t)
 '(org-hide ((t (:background "#181d23" :foreground "#181d23"))) t)
 '(py-XXX-tag-face ((t (:foreground "Firebrick"))) t)
 '(rainbow-delimiters-depth-1-face ((t (:foreground "red"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "color-160"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "color-196"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "brightred"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "color-202"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "color-208"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "color-95"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "color-33"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "color-99"))))
 '(region ((t (:background "#4271ae" :foreground "black"))))
 '(show-paren-match ((t (:background "black"))))
 '(show-paren-mismatch ((t (:background "red" :foreground "white"))))
 '(trailing-whitespace ((t (:background "color-52")))))
 '(vertical-border ((t (:foreground "black"))))
