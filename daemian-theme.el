(deftheme daemian
  "Created 2018-03-02.")

(custom-theme-set-faces
 'daemian
 '(button ((t (:inherit (link)))))
 '(cider-debug-code-overlay-face ((t (:inverse-video t))))
 '(cider-result-overlay-face ((t (:foreground "brightblue" :box (:line-width -1 :color "brightblue")))))
 '(cider-test-error-face ((t (:background "yellow" :foreground "black"))))
 '(cider-test-failure-face ((t (:background "red" :foreground "black"))))
 '(clojure-character-face ((t (:inherit (font-lock-string-face)))))
 '(clojure-keyword-face ((t (:foreground "dark cyan"))))
 '(comint-highlight-input ((t nil)))
 '(comint-highlight-prompt ((t (:foreground "brightblue"))))
 '(compilation-column-number ((t (:background "green" :foreground "black"))))
 '(compilation-info ((t (:background "purple" :foreground "white" :weight bold))))
 '(compilation-line-number ((t (:background "green" :foreground "black"))))
 '(cursor ((t (:background "#a6cafe"))))
 '(custom-variable-tag ((t (:foreground "brightblue" :weight bold))))
 '(default ((t (\.\.\. nil :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(diff-added ((t (:foreground "color-22"))))
 '(diff-changed ((t (:foreground "purple"))))
 '(diff-refine-added ((t (:background "color-22"))))
 '(diff-refine-changed ((t (:foreground "purple"))))
 '(diff-refine-removed ((t (:background "color-52"))))
 '(diredp-dir-name ((t (:inherit font-lock-function-name-face))))
 '(diredp-dir-priv ((t (:foreground "Red" :weight bold))))
 '(diredp-exec-priv ((t nil)))
 '(diredp-no-priv ((t nil)))
 '(diredp-read-priv ((t nil)))
 '(diredp-write-priv ((t nil)))
 '(ediff-current-diff-A ((t (:inherit smerge-mine))))
 '(ediff-current-diff-B ((t (:inherit smerge-other))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(flx-highlight-face ((t (:weight bold :foreground "green" :background "brightblack"))))
 '(flycheck-warning ((t (:inherit whitespace-tab))))
 '(font-lock-builtin-face ((((class grayscale) (background light)) (:weight bold :foreground "LightGray")) (((class grayscale) (background dark)) (:weight bold :foreground "DimGray")) (((class color) (min-colors 88) (background light)) (:foreground "dark slate blue")) (((class color) (min-colors 88) (background dark)) (:foreground "LightSteelBlue")) (((class color) (min-colors 16) (background light)) (:foreground "Orchid")) (((class color) (min-colors 16) (background dark)) (:foreground "LightSteelBlue")) (((class color) (min-colors 8)) (:weight bold :foreground "blue")) (t (:weight bold))))
 '(font-lock-comment-delimiter-face ((t (:foreground "brightgreen"))))
 '(font-lock-comment-face ((t (:background nil :foreground "red"))))
 '(font-lock-constant-face ((t (:foreground "blue"))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face))))
 '(font-lock-function-name-face ((t (:background "black" :foreground "brightblue"))))
 '(font-lock-keyword-face ((t (:foreground "Purple"))))
 '(font-lock-negation-char-face ((t (:foreground "color-118"))))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "color-240"))))
 '(font-lock-type-face ((((class grayscale) (background light)) (:weight bold :foreground "Gray90")) (((class grayscale) (background dark)) (:weight bold :foreground "DimGray")) (((class color) (min-colors 88) (background light)) (:foreground "ForestGreen")) (((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen")) (((class color) (min-colors 16) (background light)) (:foreground "ForestGreen")) (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen")) (((class color) (min-colors 8)) (:foreground "green")) (t (:underline t :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "color-125" :weight bold))))
 '(font-lock-warning-face ((t (:inherit (error)))))
 '(header-line ((t (:inherit mode-line :background "gray20" :weight bold))))
 '(helm-buffer-directory ((t (:inherit helm-buffer-file))))
 '(helm-selection ((t (:background "black" :foreground "brightgreen"))))
 '(helm-source-header ((t (:inherit helm-header))))
 '(highlight ((t (:background "brightblue" :foreground "black"))))
 '(highlight-symbol-face ((t (:background "color-235" :underline (:color foreground-color :style wave)))))
 '(isearch ((t (:background "purple" :foreground "white"))))
 '(isearch-fail ((t (:weight ultra-bold :foreground "white" :background "red"))))
 '(italic ((((supports :underline t)) (:underline nil))))
 '(ivy-current-match ((t (:inherit flx-highlight-face))))
 '(ivy-minibuffer-match-face-1 ((t (:inherit ido-vertical-first-match-face))))
 '(ivy-minibuffer-match-face-2 ((t (:inherit ido-vertical-match-face))))
 '(ivy-minibuffer-match-face-3 ((t (:background "color-21"))))
 '(ivy-minibuffer-match-face-4 ((t (:background "color-23"))))
 '(lazy-highlight ((t (:background "black" :foreground "purple" :weight bold))))
 '(link ((((class color) (min-colors 88) (background light)) (:underline t :foreground "RoyalBlue3")) (((class color) (background light)) (:underline t :foreground "blue")) (((class color) (min-colors 88) (background dark)) (:underline t :foreground "cyan1")) (((class color) (background dark)) (:underline t :foreground "cyan")) (t (:inherit (underline)))))
 '(link-visited ((t (:foreground "magenta4" :inherit (link)))))
 '(linum ((t (:foreground "brightblack" :background "gray3"))))
 '(magit-branch-local ((t (:foreground "blue"))))
 '(magit-branch-remote ((t (:background "black" :foreground "blue"))))
 '(magit-diff-added ((t (:foreground "green"))))
 '(magit-diff-added-highlight ((t (:background "black" :foreground "green"))))
 '(magit-diff-context ((t (:foreground "black"))))
 '(magit-diff-context-highlight ((t (:background "brightblack"))))
 '(magit-diff-file-heading ((t (:foreground "brightblue"))))
 '(magit-diff-file-heading-highlight ((t (:weight bold))))
 '(magit-diff-hunk-heading ((t (:background "blue"))))
 '(magit-diff-hunk-heading-highlight ((t (:background "color-208" :foreground "black"))))
 '(magit-diff-hunk-heading-selection ((t (:foreground "blue"))))
 '(magit-diff-lines-heading ((t (:background "color-208" :foreground "black"))))
 '(magit-diff-removed ((t (:background "black" :foreground "red"))))
 '(magit-diff-removed-highlight ((t (:background "black" :foreground "red"))))
 '(magit-hash ((t (:foreground "green"))))
 '(magit-section-heading ((t (:background "color-54" :foreground "color-225"))))
 '(magit-section-highlight ((t nil)))
 '(magit-section-secondary-heading ((t (:inherit magit-section-heading :slant italic :weight normal))))
 '(markdown-inline-code-face ((t (:inherit mode-line-read-only-face :foreground "DeepSkyBlue"))))
 '(match ((t (:background "black" :foreground "brightgreen" :weight bold))))
 '(minibuffer-prompt ((t (:foreground "#4271ae"))))
 '(mode-line ((t (:background "#4271ae" :foreground "#bbbbbb" :weight bold))))
 '(mode-line-80col-face ((t (:background "yellow" :foreground "black"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t nil)))
 '(mode-line-filename-face ((t (:foreground "brightwhite" :weight bold))))
 '(mode-line-folder-face ((t (:inherit mode-line-face :foreground "black"))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 1 :color "grey40" :style released-button) :foreground "brightgreen"))))
 '(mode-line-inactive ((t (:background "color-234" :foreground "grey20" :weight light))))
 '(mode-line-minor-mode-face ((t (:inherit mode-line-mode-face :foreground "bright black"))))
 '(mode-line-mode-face ((t (:inherit mode-line-face :foreground "color-233"))))
 '(mode-line-modified-face ((t (:inherit mode-line-face :background "#c82829" :foreground "#ffffff" :box (:line-width 2 :color "#ffffff") :weight bold))))
 '(mode-line-position-face ((t (:inherit mode-line-face))))
 '(mode-line-process-face ((t (:inherit mode-line-face :foreground "#718c00"))))
 '(mode-line-read-only-face ((t (:inherit mode-line-face :foreground "#000000"))))
 '(next-error ((t (:inherit (region)))))
 '(next-error ((t (:inherit (region)))))
 '(org-date ((t (:inherit font-lock-function-name-face :underline t))))
 '(org-done ((t (:background "color-53" :foreground "color-239" :weight bold))))
 '(org-hide ((t (:foreground "color-16"))))
 '(org-todo ((t (:background "ForestGreen" :foreground "black" :weight bold))))
 '(query-replace ((t (:inherit (isearch)))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "brightblack"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "red"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "color-202"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "brightblue"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "purple"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "violet"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "color-30"))))
 '(region ((t (:background "color-237" :foreground "#4271ae"))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:background "yellow")) (((class color) (min-colors 16) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 8)) (:foreground "black" :background "cyan")) (t (:inverse-video t))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(show-paren-match ((t (:background "color-235" :foreground "brightblue"))))
 '(show-paren-mismatch ((t (:background "red" :foreground "white"))))
 '(smerge-base ((t (:background "color-24"))))
 '(smerge-lower ((t (:background "color-34" :foreground "black"))))
 '(smerge-markers ((t (:background "brightblack"))))
 '(smerge-refined-removed ((t (:inherit smerge-refined-change :background "color-130"))))
 '(smerge-upper ((t (:background "color-208" :foreground "black"))))
 '(sml/filename ((t (:inherit mode-line-filename-face))))
 '(sml/folder ((t (:inherit mode-line-filename-face))))
 '(sml/modified ((t (:inherit mode-line-modified-face))))
 '(sml/projectile ((t (:inherit font-lock-function-name-face))))
 '(sml/read-only ((t (:inherit mode-line-read-only-face :foreground "DeepSkyBlue"))))
 '(trailing-whitespace ((t (:background "color-234"))))
 '(variable-pitch ((t (:family "Sans Serif"))))
 '(vertical-border ((t (:foreground "black"))))
 '(whitespace-line ((t (:underline t))))
 '(whitespace-tab ((t (:background "color-52"))))
 '(whitespace-trailing ((t (:inherit whitespace-tab :weight bold))))
 '(widget-field ((t (:background "color-21" :foreground "color-254")))))

(provide-theme 'daemian)
