(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(buffer-stack-untracked (quote ("KILL" "*Compile-Log*" "*Ibuffer*" "*Compile-Log-Show*" "*Group*" "*Completions*" "*Kill Ring*" "*Messages*" "*nav*")))
 '(column-number-mode t)
 '(display-time-day-and-date t)
 '(display-time-mail-face nil)
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(grep-highlight-matches t)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(hippie-expand-try-functions-list (quote (try-expand-dabbrev try-expand-line try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list)))
 '(hl-paren-background-colors (quote ("color-57")))
 '(hl-paren-colors (quote ("brightwhite" "IndianRed1" "IndianRed3" "IndianRed4")))
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(large-file-warning-threshold nil)
 '(linum-format "%d ")
 '(list-matching-lines-face (quote default))
 '(magit-commit-all-when-nothing-staged nil)
 '(magit-process-popup-time 10)
 '(mode-line-in-non-selected-windows t)
 '(mumamo-chunk-coloring 0 t)
 '(nav-bookmark-list (quote ("~/src/git/cloud-quote" "~/.emacs.d" "/tmp" "/")))
 '(nav-default-width 30)
 '(nav-follow t)
 '(nav-quickdir-list (quote ("~/src/git/cloud-quote" "~/src/git/cloud-ui" "~/.emacs.d")))
 '(nav-quickfile-list (quote ("~/dotfiles/.bashrc" "" "")))
 '(nav-quickjump-show nil)
 '(nav-widths-percentile 99)
 '(newsticker-automatically-mark-items-as-old nil)
 '(newsticker-desc-format " - -  %c")
 '(newsticker-frontend (quote newsticker-plainview))
 '(newsticker-heading-format "[ %t %d ]")
 '(newsticker-html-renderer (quote w3m-region))
 '(newsticker-item-format " .  %t")
 '(newsticker-justification nil)
 '(newsticker-obsolete-item-max-age 2286400)
 '(newsticker-retrieval-interval 60)
 '(newsticker-sort-method (quote sort-by-time))
 '(newsticker-treeview-listwindow-height 20)
 '(newsticker-url-list (quote (("moox" "https://trac.fmpub.net/FMP/timeline?ticket=on&milestone=on&changeset=on&repo-=on&repo-BigTent=on&repo-DataServices=on&repo-FMCM=on&repo-SemTech=on&repo-daily_buzz_content=on&repo-daily_buzz_luxe=on&repo-daily_buzz_moms=on&repo-daily_buzz_newsletter=on&repo-daily_buzz_style=on&repo-daily_buzz_user=on&repo-daily_buzz_user_service=on&repo-fbz_foodbuzz=on&repo-fbz_miner=on&repo-fbz_web=on&wiki=on&max=50&authors=&daysback=90&format=rss" nil nil ("--user=dmack@federatedmedia.net" "--password=fa!cl4m4t0juic3")) ("nvogel|Data Services" "https://trac.fmpub.net/FMP/timeline?ticket=on&milestone=on&changeset=on&repo-=on&repo-BigTent=on&repo-BigTent_Video=on&repo-BigTent_cruisecontrol=on&repo-BigTent_httpunit=on&repo-BigTent_social_media=on&repo-BigTent_tackynotes=on&repo-BigTent_unittests=on&repo-BigTent_ygma=on&repo-DataServices=on&repo-FMCM=on&repo-FMP_finance=on&repo-FMP_vendors=on&repo-QA=on&repo-SemTech=on&repo-daily_buzz_content=on&repo-daily_buzz_luxe=on&repo-daily_buzz_moms=on&repo-daily_buzz_newsletter=on&repo-daily_buzz_style=on&repo-daily_buzz_user=on&repo-daily_buzz_user_service=on&repo-fbz_foodbuzz=on&repo-fbz_harvester=on&repo-fbz_iphone=on&repo-fbz_miner=on&repo-fbz_miner2=on&repo-fbz_playpen=on&repo-fbz_publisher=on&repo-fbz_sifter=on&repo-fbz_web=on&wiki=on&max=50&authors=nvogel%40federatedmedia.net&daysback=90&format=rss" nil nil nil))))
 '(newsticker-url-list-defaults (quote (("Emacs Wiki" "http://www.emacswiki.org/cgi-bin/wiki.pl?action=rss" nil 3600))))
 '(newsticker-use-full-width nil)
 '(org-agenda-custom-commands (quote (("x" "STUFF!" agenda "" nil))) t)
 '(org-agenda-files (quote ("~/Desktop/notes.org")))
 '(org-agenda-ndays 14)
 '(org-cycle-global-at-bob t)
 '(org-hide-leading-stars t)
 '(org-odd-levels-only nil)
 '(org-special-ctrl-a/e (quote reversed))
 '(recentf-max-saved-items 100)
 '(recentf-save-file "~/.emacs.d/.recentf")
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(show-trailing-whitespace t)
 '(size-indication-mode t)
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
 '(button ((((supports :underline t)) nil)))
 '(comint-highlight-prompt ((t (:foreground "brightblue"))))
 '(custom-comment-tag ((((class color) (background light)) (:foreground "brightblue"))))
 '(custom-group-tag ((((min-colors 88) (class color) (background light)) (:foreground "brightblue" :weight bold :height 1.2))))
 '(custom-variable-tag ((((min-colors 88) (class color) (background light)) (:inherit variable-pitch :foreground "brightblue" :weight bold :height 1.2))))
 '(cycbuf-current-face ((t (:background "color-52" :weight bold))))
 '(ediff-current-diff-B ((((class color) (min-colors 16)) (:background "Yellow" :foreground "black"))))
 '(ediff-even-diff-A ((((class color) (min-colors 16)) (:foreground "brightblue"))))
 '(ediff-even-diff-Ancestor ((((class color) (min-colors 16)) (:background "brightblue" :foreground "White"))))
 '(ediff-even-diff-B ((((class color) (min-colors 16)) (:foreground "brightblue"))))
 '(ediff-fine-diff-Ancestor ((((class color) (min-colors 16)) (:background "brightblack" :foreground "white"))))
 '(ediff-odd-diff-A ((((class color) (min-colors 16)) (:background "brightblack" :foreground "white"))))
 '(egoge-display-time ((((type tty)) (:background "black" :foreground "color-130"))))
 '(flymake-errline ((((class color) (background light)) (:background "red" :foreground "brightwhite" :weight bold))))
 '(font-lock-function-name-face ((((class color) (min-colors 88) (background light)) (:foreground "brightblue"))))
 '(header-line ((default (:background "#222222" :foreground "white")) (((type tty)) nil)))
 '(hi-blue-b ((((min-colors 88)) (:foreground "brightblue" :weight bold))))
 '(highlight ((((class color) (min-colors 88) (background light)) (:background "color-233"))))
 '(hl-line ((t (:background "color-233"))))
 '(hl-paren-face ((t nil)) t)
 '(isearch ((((class color) (min-colors 88) (background light)) (:background "blue" :foreground "white" :underline "white" :weight extra-bold))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "red" :foreground "black" :strike-through "yellow"))))
 '(italic ((((supports :underline t)) (:underline nil))))
 '(lazy-highlight ((t (:background "blue" :foreground "black" :underline "white" :weight bold))))
 '(link ((((class color) (min-colors 88) (background light)) (:foreground "brightblue" :underline t))))
 '(magit-diff-file-header ((t (:background "green"))))
 '(magit-diff-hunk-header ((t (:inherit magit-header :background "blue" :foreground "white" :slant italic))))
 '(magit-diff-none ((t (:foreground "color-235"))))
 '(magit-header ((t (:foreground "color-45"))))
 '(magit-item-highlight ((t (:background "black"))))
 '(magit-section-title ((t (:inherit magit-header :foreground "red" :weight bold))))
 '(match ((nil (:background "black" :foreground "white" :weight bold))))
 '(minibuffer-prompt ((t (:foreground "brightblue"))))
 '(mode-line ((t (:background "brightblue" :foreground "black" :inverse_video nil :box (:line-width -1 :style released-button)))))
 '(mode-line-buffer-id ((t nil)))
 '(mode-line-emphasis ((t nil)))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:foreground "brightgreen" :box (:line-width 1 :color "grey40" :style released-button)))))
 '(mode-line-inactive ((default (:foreground "brightblack")) (nil nil)))
 '(monky-diff-add ((t (:background "color-232" :foreground "color-22"))))
 '(monky-diff-none ((t (:foreground "brightblack"))))
 '(mumamo-background-chunk-major ((t nil)))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background light)) nil)))
 '(mumamo-background-chunk-submode2 ((((class color) (min-colors 88) (background light)) nil)))
 '(mumamo-background-chunk-submode3 ((((class color) (min-colors 88) (background light)) nil)))
 '(newsticker-feed-face ((((class color) (background dark)) (:foreground "salmon" :height 1.2 :family "helvetica"))))
 '(newsticker-new-item-face ((nil (:background "brightblack" :foreground "black"))))
 '(newsticker-obsolete-item-face ((((class color) (background dark)) (:strike-through t :family "helvetica"))))
 '(newsticker-old-item-face ((t (:underline nil :family "helvetica"))))
 '(newsticker-statistics-face ((((class color) (background dark)) (:foreground "lightblue" :slant italic :height 0.8 :family "helvetica"))))
 '(newsticker-treeview-new-face ((((class color) (background dark)) (:inherit newsticker-treeview-face :foreground "orange3" :weight bold))))
 '(newsticker-treeview-selection-face ((((class color) (background dark)) (:background "#bbbbff" :foreground "black"))))
 '(org-done ((((class color) (min-colors 16) (background dark)) (:foreground "Royalblue"))))
 '(org-hide ((t (:foreground "grey25"))))
 '(org-level-1 ((t (:background "cornflowerblue" :foreground "white" :weight regular))))
 '(org-level-2 ((t (:background "selectedKnobColor" :foreground "white" :weight regular))))
 '(org-level-3 ((t (:background "KnobColor" :foreground "white" :weight regular))))
 '(org-link ((((class color) (background dark)) (:foreground "cornflowerblue" :underline t))))
 '(org-todo ((((class color) (min-colors 16) (background dark)) (:background "black" :foreground "chartreuse" :weight bold))))
 '(py-XXX-tag-face ((t (:foreground "Firebrick"))) t)
 '(region ((t (:background "blue" :foreground "white"))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:background "yellow1" :foreground "black"))))
 '(set-cursor-color (quote black))
 '(show-paren-match ((t (:background "#444444" :foreground "black" :overline nil :underline nil :slant normal :weight normal))))
 '(trailing-whitespace ((((class color) (background dark)) (:background "color-234"))))
 '(underline ((((supports :underline t)) (:underline nil))))
 '(vertical-border ((((type tty)) (:background "black" :foreground "brightblack"))))
 '(which-func ((t (:inherit magit-item-highlight)))))
