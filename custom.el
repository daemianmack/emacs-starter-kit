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
 '(browse-kill-ring-display-duplicates nil)
 '(browse-kill-ring-highlight-current-entry t)
 '(browse-kill-ring-separator
   ".............................................................................")
 '(browse-kill-ring-separator-face (quote hi-black-b))
 '(buffer-stack-untracked
   (quote
    ("KILL" "*Compile-Log*" "*Ibuffer*" "*Compile-Log-Show*" "*Group*" "*Completions*" "*Kill Ring*" "*Messages*" "*nav*")))
 '(cider-popup-on-error t)
 '(cider-prompt-save-file-on-load nil)
 '(cider-repl-history-file nil)
 '(cider-repl-result-prefix "=> ")
 '(cider-repl-use-clojure-font-lock t)
 '(cider-show-error-buffer nil)
 '(column-number-mode t)
 '(comment-auto-fill-only-comments t)
 '(cursor-color "#626262")
 '(custom-safe-themes
   (quote
    ("c9ea3e0172ac29e7ead0066b98329dd4c860945e19b52f7b72fd593e8a14d72e" "cd406eb9d928913fdcab1409c490ded9001da85022169dd49bf68dc5e060ad06" "f9a7e495f09c5abb62f74e4f1d9eb122fb9831072817679c1c5058976fcbc58f" "af0092d6fc8dca639e505724f2e79fb4ae25e39386bf676a42fe9d4e41cdc89f" "d304fbd3874beff28078b60133a0791677c06de26b234defa48ca39286b69d27" "32c0420ef0b80758018e86496355fcc7335372cda1843ea7e46174d3d84988ec" "b85cf3c59bcb6334b7fb8bcd154e235480868d63372df90b30a67d022efdda0f" "fae57ad3316e05b1f61b0cdcaa2eb251e9292f5f8c34ee7e56c8633a8d44339d" "d5a1d188bf45b95b764ae302436ced7e1b83e8230053c8cab93aab941047845f" "b34552e26c4920105ad013afbb6177867edb1b80789d707f3e3a2a9427ffb24d" "af15bbd874718e8eec17e8a921cfa43894b2713685a2e6b2e21e6d6d4c6cb9ca" "fb4d64b1adbfcbea80b029c03caf92fd4da4d8b3248050b0d1e08edd6520bbaf" "60211629adfd0c1752a3185ff9d56cbc401507036778b34591bd40d0152ee41e" "67131a19c7daa41baa949100718c097603c084307f86e278521dde54fa15bbf9" "a0113f4796d5aa45d45c4252f961213a8adc9fe95ca23dbe07a57d15049ed1df" "ddbc7cdc87a26302db9a4eeb9ad96f0415f085b65f2eec7a2ffef03329a0cb76" "967c58175840fcea30b56f2a5a326b232d4939393bed59339d21e46cf4798ecf" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "5d98db0ee0af3681d987fc0a9ca3d32632cf18b73c50238fe9ba5a58ed797d93" "8ac31e1bc1920b33d478dfafb0b45989a00ede15a2388ea16093e7d0988c48d0" "01680272822bbb2d02f85f4fefb0684ab15bbd5b229ddfd4929ddececcec6d6c" "548f6cea041842d145ee35b90f1a07a74a51b14f77f1e931138f960c42ca7b39" "bfc0f14c5d5902b5154d57c6d3d14f8389b74997708aae3e98315795a0f0ca6b" "4a4475800ef69d5b3b6e02cd88a3fef391b2fb9bce5c7c9bfcb065cfa6ebce33" "e35ac8101749aa8ac01b588418a134d9ad42a74d5a1619228fe5fd4ac5f44898" "aaed3c12b6121161c0e3d142c6e25002e062fdfc47da26b5fd73c1f3b3eae0e0" "3223b32123d0890c8aa1674db7b6d84f0d31b70c7411a9ffd455ebfbcb3a5a82" "7400b80c7478b262314f9176cd616fe19b4457b67156145b4977a4b260216a32" "181bf8c5f71bb208ecd6a632fa6fa8cd86060dfbdd8fa82d5eae7df0c7c7b90e" "c2a2175e9ae03e328c1877f4b60bb0c1f78f796a2f433edf700acb41f7b1fec2" "34a0f577803a27674e5ae6bb2d82394dafd5252d9df751868b3821e02d0c5755" "ac24ba4e5adddd53aee6d4fd029a77ebf09920d59418e7a70d8a41ce7b78f9d6" "acca6c7438c8cc67b6b45da2841ad3452e2d9ab05b24eb8bafd2cd0985cf8ddc" "8340250882a74e751421b85cd18f277ab25c95fd63daa800ecbd42d79fc74f0f" "159bb8f86836ea30261ece64ac695dc490e871d57107016c09f286146f0dae64" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "e9a1226ffed627ec58294d77c62aa9561ec5f42309a1f7a2423c6227e34e3581" "951e10f17de57de1e0c9cbeb44fcdda1b6c6d26beab40c3bd0abbfc38dd5c9c8" "bf7ed640479049f1d74319ed004a9821072c1d9331bc1147e01d22748c18ebdf" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(desktop-after-read-hook nil)
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
     (read-expression-history . 60)
     (regexp-history . 60)
     (regexp-search-ring . 20)
     (search-ring . 20)
     (shell-command-history . 50)
     (kill-ring . 20)
     tags-file-name register-alist)))
 '(desktop-load-locked-desktop nil)
 '(desktop-save t)
 '(desktop-save-mode t)
 '(diff-switches "-u")
 '(display-time-day-and-date t)
 '(display-time-mail-face nil)
 '(fci-rule-character-color "#192028")
 '(fci-rule-color "#383838")
 '(flx-ido-mode t)
 '(foreground-color "#626262")
 '(global-highlight-parentheses-mode t)
 '(global-linum-mode t)
 '(global-rainbow-delimiters-mode t)
 '(grep-highlight-matches t)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(helm-buffer-max-length 80)
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
 '(kill-ring-max 200)
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
 '(pe/side (quote left))
 '(projectile-ack-function (quote ack))
 '(projectile-completion-system (quote ido))
 '(projectile-global-mode t)
 '(projectile-mode-line
   (quote
    (:eval
     (format " Proj[%s]"
	     (projectile-project-name)))))
 '(projectile-project-root-files-bottom-up
   (quote
    (".projectile" ".git" ".hg" ".fslckout" ".bzr" "_darcs")))
 '(projectile-remember-window-configs t)
 '(recentf-max-saved-items 100)
 '(recentf-save-file (concat dotfiles-dir ".recentf"))
 '(safe-local-variable-values
   (quote
    ((buffer-file-coding-system . utf-8-unix)
     (org-html-head-include-scripts)
     (org-html-head-include-default-style)
     (eval when
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
 '(diff-refine-added ((t nil)))
 '(diff-refine-removed ((t nil)))
 '(magit-diff-add ((t (:foreground "green"))))
 '(magit-diff-del ((t (:foreground "red")))))
