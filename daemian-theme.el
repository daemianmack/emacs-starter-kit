(deftheme daemian
  "Created 2020-08-02.")

(custom-theme-set-variables
 'daemian
 '(iflipb-current-buffer-template "%s")
 '(smartrep-mode-line-active-bg "#4271ae"))

(custom-theme-set-faces
 'daemian
 '(button ((t (:inherit (link)))))
 '(cider-debug-code-overlay-face ((t (:inverse-video t))))
 '(cider-result-overlay-face ((t (:foreground "#5c5cff" :box (:line-width -1 :color "#5c5cff")))))
 '(cider-test-error-face ((t (:background "#cdcd00" :foreground "#000000"))))
 '(cider-test-failure-face ((t (:background "#cd0000" :foreground "#000000"))))
 '(clojure-character-face ((t (:inherit (font-lock-string-face)))))
 '(clojure-keyword-face ((t (:foreground "#008b8b"))))
 '(comint-highlight-input ((t nil)))
 '(comint-highlight-prompt ((t (:foreground "#5c5cff"))))
 '(compilation-column-number ((t (:background "#00cd00" :foreground "#000000"))))
 '(compilation-info ((t (:background "#a020f0" :foreground "#e5e5e5" :weight bold))))
 '(compilation-line-number ((t (:background "#00cd00" :foreground "#000000"))))
 '(cursor ((t (:background "#00cd00"))))
 '(custom-variable-tag ((t (:foreground "#5c5cff" :weight bold))))
 '(diff-added ((t (:foreground "#005f00"))))
 '(diff-changed ((t (:foreground "#a020f0"))))
 '(diff-refine-added ((t (:background "#005f00"))))
 '(diff-refine-changed ((t (:foreground "#a020f0"))))
 '(diff-refine-removed ((t (:background "#5f0000"))))
 '(escape-glyph ((((background dark)) (:foreground "#00cdcd")) (((type pc)) (:foreground "#cd00cd")) (t (:foreground "#a52a2a"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(flx-highlight-face ((t (:weight bold :foreground "#00cd00" :background "#1c1c1c"))))
 '(flycheck-warning ((t (:inherit whitespace-tab))))
 '(font-lock-builtin-face ((((class grayscale) (background light)) (:weight bold :foreground "#d3d3d3")) (((class grayscale) (background dark)) (:weight bold :foreground "#696969")) (((class color) (min-colors 88) (background light)) (:foreground "#483d8b")) (((class color) (min-colors 88) (background dark)) (:foreground "#b0c4de")) (((class color) (min-colors 16) (background light)) (:foreground "#da70d6")) (((class color) (min-colors 16) (background dark)) (:foreground "#b0c4de")) (((class color) (min-colors 8)) (:weight bold :foreground "#0000ee")) (t (:weight bold))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#00ff00"))))
 '(font-lock-comment-face ((t (:background nil :foreground "#cd0000"))))
 '(font-lock-constant-face ((t (:foreground "#4876ff"))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face))))
 '(font-lock-function-name-face ((t (:background "#000000" :foreground "#4876ff"))))
 '(font-lock-keyword-face ((t (:foreground "#a020f0"))))
 '(font-lock-negation-char-face ((t (:foreground "#87ff00"))))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#585858"))))
 '(font-lock-type-face ((((class grayscale) (background light)) (:weight bold :foreground "#e5e5e5")) (((class grayscale) (background dark)) (:weight bold :foreground "#696969")) (((class color) (min-colors 88) (background light)) (:foreground "#228b22")) (((class color) (min-colors 88) (background dark)) (:foreground "#98fb98")) (((class color) (min-colors 16) (background light)) (:foreground "#228b22")) (((class color) (min-colors 16) (background dark)) (:foreground "#98fb98")) (((class color) (min-colors 8)) (:foreground "#00cd00")) (t (:underline t :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "#af005f" :weight bold))))
 '(font-lock-warning-face ((t (:inherit error))))
 '(header-line ((t (:inherit mode-line :background "#333333" :weight bold))))
 '(helm-buffer-directory ((t (:inherit helm-buffer-file))))
 '(helm-selection ((t (:background "#000000" :foreground "#00ff00"))))
 '(helm-source-header ((t (:inherit helm-header))))
 '(highlight ((t (:background "#5c5cff" :foreground "#000000"))))
 '(iflipb-current-buffer-face ((t (:inherit font-lock-function-name-face))))
 '(isearch ((t (:background "#a020f0" :foreground "#e5e5e5"))))
 '(isearch-fail ((t (:weight ultra-bold :foreground "#e5e5e5" :background "#cd0000"))))
 '(italic ((((supports :underline t)) (:underline nil))))
 '(ivy-current-match ((t (:inherit flx-highlight-face))))
 '(ivy-minibuffer-match-face-1 ((t nil)))
 '(ivy-minibuffer-match-face-2 ((t (:foreground "#a020f0"))))
 '(ivy-minibuffer-match-face-3 ((t (:foreground "#a020f0"))))
 '(ivy-minibuffer-match-face-4 ((t (:foreground "#a020f0"))))
 '(lazy-highlight ((t (:background "#000000" :foreground "#a020f0" :weight bold))))
 '(link ((((class color) (min-colors 88) (background light)) (:underline t :foreground "#3a5fcd")) (((class color) (background light)) (:underline t :foreground "#0000ee")) (((class color) (min-colors 88) (background dark)) (:underline t :foreground "#00ffff")) (((class color) (background dark)) (:underline t :foreground "#00cdcd")) (t (:inherit (underline)))))
 '(link-visited ((t (:foreground "#8b008b" :inherit (link)))))
 '(magit-branch-local ((t (:foreground "#4876ff"))))
 '(magit-branch-remote ((t (:background "#000000" :foreground "#4876ff"))))
 '(magit-diff-added ((t (:foreground "#00cd00"))))
 '(magit-diff-added-highlight ((t (:background "#000000" :foreground "#00cd00"))))
 '(magit-diff-context ((t (:foreground "#1a1a1a"))))
 '(magit-diff-context-highlight ((t (:background "#000000"))))
 '(magit-diff-file-heading ((t (:foreground "#5c5cff"))))
 '(magit-diff-file-heading-highlight ((t (:weight bold))))
 '(magit-diff-hunk-heading ((t (:background "#4876ff" :foreground "#000000" :weight bold))))
 '(magit-diff-hunk-heading-highlight ((t (:background "#4876ff" :foreground "#e5e5e5" :weight bold))))
 '(magit-diff-hunk-heading-selection ((t (:foreground "#4876ff"))))
 '(magit-diff-lines-heading ((t (:background "#ff8700" :foreground "#000000"))))
 '(magit-diff-removed ((t (:background "#000000" :foreground "#cd0000"))))
 '(magit-diff-removed-highlight ((t (:background "#000000" :foreground "#cd0000"))))
 '(magit-diff-revision-summary ((t (:inherit magit-diff-hunk-heading :foreground "#000000"))))
 '(magit-hash ((t (:foreground "#00cd00"))))
 '(magit-section-heading ((t (:background "#68228b" :foreground "#f5f5f5"))))
 '(magit-section-highlight ((t nil)))
 '(magit-section-secondary-heading ((t (:inherit magit-section-heading :slant italic :weight normal))))
 '(match ((t (:background "#000000" :foreground "#00ff00" :weight bold))))
 '(minibuffer-prompt ((t (:foreground "#4271ae"))))
 '(mode-line ((t (:background "#4271ae" :foreground "#bbbbbb" :weight bold))))
 '(mode-line-80col-face ((t (:background "#cdcd00" :foreground "#000000"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t nil)))
 '(mode-line-filename-face ((t (:foreground "#ffffff" :weight bold))))
 '(mode-line-folder-face ((t (:inherit mode-line-face :foreground "#000000"))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 1 :color "#666666" :style released-button) :foreground "#00ff00"))))
 '(mode-line-inactive ((t (:background "#1c1c1c" :foreground "#333333" :weight light))))
 '(mode-line-minor-mode-face ((t (:inherit mode-line-mode-face :foreground "#7f7f7f"))))
 '(mode-line-mode-face ((t (:inherit mode-line-face :foreground "#121212"))))
 '(mode-line-modified-face ((t (:inherit mode-line-face :background "#c82829" :foreground "#ffffff" :box (:line-width 2 :color "#ffffff") :weight bold))))
 '(mode-line-position-face ((t (:inherit mode-line-face))))
 '(mode-line-process-face ((t (:inherit mode-line-face :foreground "#718c00"))))
 '(mode-line-read-only-face ((t (:inherit mode-line-face :foreground "#000000"))))
 '(next-error ((t (:inherit (region)))))
 '(next-error ((t (:inherit (region)))))
 '(org-date ((t (:inherit font-lock-function-name-face :underline t))))
 '(org-done ((t (:background "#5f005f" :foreground "#4e4e4e" :weight bold))))
 '(org-hide ((t (:foreground "#000000"))))
 '(org-todo ((t (:background "#228b22" :foreground "#000000" :weight bold))))
 '(query-replace ((t (:inherit (isearch)))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#1c1c1c"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#cd0000"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#ffa500"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#ff5f00"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#00cd00"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#5c5cff"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#a020f0"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#ee82ee"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#008787"))))
 '(region ((t (:background "#262626" :foreground "#4271ae"))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:background "#ffff00")) (((class color) (min-colors 88) (background dark)) (:background "#4a708b")) (((class color) (min-colors 16) (background light)) (:background "#cdcd00")) (((class color) (min-colors 16) (background dark)) (:background "#4a708b")) (((class color) (min-colors 8)) (:foreground "#000000" :background "#00cdcd")) (t (:inverse-video t))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "#7f7f7f")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "#b3b3b3")) (((class color) (min-colors 8) (background light)) (:foreground "#00cd00")) (((class color) (min-colors 8) (background dark)) (:foreground "#cdcd00"))))
 '(show-paren-match ((t (:background "#2b2b2b" :foreground "#5c5cff"))))
 '(show-paren-mismatch ((t (:background "#cd0000" :foreground "#e5e5e5"))))
 '(smerge-base ((t (:background "#005f87"))))
 '(smerge-lower ((t (:background "#00af00" :foreground "#000000"))))
 '(smerge-markers ((t (:background "#1c1c1c"))))
 '(smerge-refined-removed ((t (:inherit smerge-refined-change :background "#af5f00"))))
 '(smerge-upper ((t (:background "#ff8700" :foreground "#000000"))))
 '(sml/filename ((t (:inherit mode-line-filename-face))))
 '(sml/folder ((t (:inherit mode-line-filename-face))))
 '(sml/modified ((t (:inherit mode-line-modified-face))))
 '(sml/read-only ((t (:inherit mode-line-read-only-face :foreground "#00bfff"))))
 '(trailing-whitespace ((t (:background "#1c1c1c"))))
 '(variable-pitch ((t (:family "Sans Serif"))))
 '(vertical-border ((t (:foreground "#321051"))))
 '(whitespace-line ((t (:underline t))))
 '(whitespace-tab ((t (:background "#5f0000"))))
 '(whitespace-trailing ((t (:inherit whitespace-tab :weight bold))))
 '(widget-field ((t (:background "#0000ff" :foreground "#e4e4e4"))))
 '(cider-error-highlight-face ((t (:inherit font-lock-warning-face :underline t))))
 '(line-number ((t (:foreground "#1c1c1c"))))
 '(diff-hl-change ((t (:foreground "#a020f0" :weight bold))))
 '(diff-hl-delete ((t (:inherit diff-removed))))
 '(diff-removed ((t (:foreground "#cd0000" :weight bold))))
 '(diff-hl-insert ((t (:inherit diff-added :foreground "#008b00" :weight bold))))
 '(logview-level-debug ((t nil)))
 '(logview-information-entry ((t nil)))
 '(logview-error-entry ((t (:background "#af0000"))))
 '(logview-level-information ((t (:inherit (success)))))
 '(org-checkbox ((t (:foreground "#1c1c1c"))))
 '(org-inflight-face ((t (:background "#ffa500" :foreground "#cd0000"))))
 '(annotate-highlight ((t (:background "#0000ee"))))
 '(annotate-annotation ((t (:background "#5f5fff" :foreground "#000000"))))
 '(tab-bar-tab ((t (:inherit tab-bar :background "#ff5f00" :foreground "#e5e5e5" :weight bold))))
 '(tab-bar-tab-inactive ((t (:background "#000000" :foreground "#a020f0"))))
 '(tab-bar ((t (:foreground "#000000"))))
 '(whitespace-space ((t nil)))
 '(symbol-overlay-default-face ((t (:background "#1f1f1f"))))
 '(line-number-current-line ((t (:background "#1c1c1c" :foreground "#00af00"))))
 '(rainbow-delimiters-base-error-face ((t (:inherit rainbow-delimiters-base-face :background "#cd0000" :foreground "#ffffff"))))
 '(helm-swoop-target-word-face ((t (:weight bold))))
 '(helm-swoop-line-number-face ((t (:inherit line-number-current-line))))
 '(helm-swoop-target-line-face ((t (:inherit isearch))))
 '(treemacs-git-modified-face ((t (:foreground "#8a2be2"))))
 '(treemacs-git-added-face ((t (:inherit (font-lock-type-face)))))
 '(treemacs-git-untracked-face ((t (:inherit (font-lock-string-face)))))
 '(treemacs-git-ignored-face ((t (:inherit (font-lock-comment-face)))))
 '(ivy-minibuffer-match-highlight ((t (:inherit (highlight)))))
 '(hl-line ((t (:extend t :background "#1c1c1c" :weight bold))))
 '(default ((t (\. \. \. nil :weight normal :height 1 :width normal :foundry "default" :family "default")))))

(provide-theme 'daemian)
