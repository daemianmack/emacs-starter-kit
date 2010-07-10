(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(global-hl-line-mode t)
 '(grep-highlight-matches t)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(large-file-warning-threshold nil)
 '(list-matching-lines-face (quote default))
 '(magit-commit-all-when-nothing-staged nil)
 '(magit-process-popup-time 10)
 '(mumamo-chunk-coloring 0 t)
 '(nav-bookmark-list (quote ("~/src/git/cloud-quote" "~/.emacs.d" "/tmp" "/")))
 '(nav-default-width 30)
 '(nav-follow t)
 '(nav-quickdir-list (quote ("~/src/git/cloud-quote" "~/src/git/cloud-ui" "~/.emacs.d")))
 '(nav-quickfile-list (quote ("~/dotfiles/.bashrc" "" "")))
 '(nav-quickjump-show nil)
 '(nav-widths-percentile 99)
 '(recentf-max-saved-items 100)
 '(recentf-save-file "~/.emacs.d/.recentf")
 '(show-trailing-whitespace t)
 '(sr-virtual-listing-switches "-aldp")
 '(uniquify-ask-about-buffer-names-p nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(uniquify-ignore-buffers-re nil)
 '(uniquify-min-dir-content 3)
 '(uniquify-separator nil)
 '(windmove-wrap-around t)
 '(uniquify-trailing-separator-p nil))

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
 '(font-lock-function-name-face ((((class color) (min-colors 88) (background light)) (:foreground "brightblue"))))
 '(header-line ((default (:background "brightblue")) (((type tty)) nil)))
 '(hi-blue-b ((((min-colors 88)) (:foreground "brightblue" :weight bold))))
 '(highlight ((((class color) (min-colors 88) (background light)) (:background "color-233"))))
 '(hl-line ((t (:background "color-233"))))
 '(isearch ((((class color) (min-colors 88) (background light)) (:background "blue" :foreground "white" :underline "white" :weight extra-bold))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "red" :foreground "black" :strike-through "yellow"))))
 '(italic ((((supports :underline t)) (:underline nil))))
 '(lazy-highlight ((t (:background "blue" :foreground "black" :underline "white" :weight bold))))
 '(link ((((class color) (min-colors 88) (background light)) (:foreground "brightblue" :underline t))))
 '(magit-diff-file-header ((t (:background "green"))))
 '(magit-diff-hunk-header ((t (:inherit magit-header :background "blue" :foreground "white" :slant italic))))
 '(magit-diff-none ((t (:foreground "color-235"))))
 '(magit-header ((t (:foreground "color-45"))))
 '(magit-section-title ((t (:inherit magit-header :foreground "red" :weight bold))))
 '(match ((nil (:background "black" :foreground "white" :weight bold))))
 '(minibuffer-prompt ((t (:foreground "brightblue"))))
 '(mode-line ((t (:background "brightblack" :foreground "white" :box (:line-width -1 :style released-button)))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:foreground "brightgreen" :box (:line-width 1 :color "grey40" :style released-button)))))
 '(mode-line-inactive ((default (:foreground "brightblack")) (nil nil)))
 '(mumamo-background-chunk-major ((t nil)))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background light)) nil)))
 '(mumamo-background-chunk-submode2 ((((class color) (min-colors 88) (background light)) nil)))
 '(mumamo-background-chunk-submode3 ((((class color) (min-colors 88) (background light)) nil)))
 '(py-XXX-tag-face ((t (:foreground "Firebrick"))) t)
 '(region ((t (:background "blue" :foreground "#dddddd"))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:background "yellow1" :foreground "black"))))
 '(trailing-whitespace ((((class color) (background dark)) (:background "color-234"))))
 '(underline ((((supports :underline t)) (:underline nil))))
 '(vertical-border ((((type tty)) (:background "black" :foreground "brightblack")))))
