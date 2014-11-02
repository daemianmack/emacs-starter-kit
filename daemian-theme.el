(deftheme daemian
  "Created 2014-11-02.")

(custom-theme-set-variables
 'daemian
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector ["#000000" "#ff0000" "#00ff00" "#ffff00" "#0000ff" "#dc8cc3" "#93e0e3" "#dcdccc"]))

(custom-theme-set-faces
 'daemian
 '(clojure-test-error-face ((t (:background "orange1" :foreground "black" :weight bold))))
 '(clojure-test-failure-face ((t (:background "orange red" :foreground "black" :weight bold))))
 '(clojure-test-success-face ((t (:foreground "green"))))
 '(comint-highlight-prompt ((t (:foreground "brightblue"))))
 '(cycbuf-current-face ((t (:background "color-52" :weight bold))))
 '(diff-added ((t (:foreground "green4"))))
 '(diff-changed ((t (:foreground "purple"))))
 '(diff-indicator-removed ((t (:inherit diff-removed))))
 '(diff-refine-added ((t (:inherit diff-refine-change))))
 '(diff-refine-change ((t (:foreground "purple"))))
 '(diff-refine-removed ((t (:inherit diff-refine-change))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red3"))))
 '(egoge-display-time ((((type tty)) (:background "black" :foreground "color-130"))))
 '(flx-highlight-face ((t (:inherit isearch-lazy-highlight-face :background "null" :weight normal))))
 '(font-lock-comment-face ((t (:background nil :foreground "red"))))
 '(highlight ((t (:background "brightblue" :foreground "white"))))
 '(highlight-symbol-face ((t (:background "red"))))
 '(hiwin-face ((t (:background "black"))))
 '(hl-paren-face ((t (:underline "red"))))
 '(italic ((((supports :underline t)) (:underline nil))))
 '(linum-highlight-face ((t (:inherit default :background "green" :foreground "black"))))
 '(magit-diff-file-header ((t (:background "black"))))
 '(magit-diff-hunk-header ((t (:background "blue" :foreground "white"))))
 '(magit-diff-none ((t (:foreground "color-235"))))
 '(mumamo-background-chunk-major ((t nil)))
 '(mumamo-background-chunk-submode2 ((((class color) (min-colors 88) (background light)) nil)))
 '(mumamo-background-chunk-submode3 ((((class color) (min-colors 88) (background light)) nil)))
 '(org-hide ((t (:foreground "color-16"))))
 '(org-level-1 ((t (:background "black" :foreground "brightblue"))))
 '(org-level-2 ((t (:foreground "color-21"))))
 '(org-level-3 ((t (:inherit org-level-2))))
 '(org-level-4 ((t (:inherit org-level-2))))
 '(org-level-5 ((t (:inherit org-level-2))))
 '(org-level-6 ((t (:inherit org-level-2))))
 '(org-level-7 ((t (:inherit org-level-2))))
 '(org-level-8 ((t (:inherit org-level-2))))
 '(py-XXX-tag-face ((t (:foreground "Firebrick"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "red"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "color-196"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "brightred"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "color-202"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "color-208"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "color-95"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "color-33"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "color-99"))))
 '(show-paren-match ((t (:background "black"))))
 '(show-paren-mismatch ((t (:background "red" :foreground "white"))))
 '(vertical-border ((t (:foreground "black"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "color-160"))))
 '(cursor ((t (:background "#a6cafe"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((t (:family "Sans Serif"))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((t (:foreground "#4271ae"))))
 '(region ((t (:background "color-237" :foreground "#4271ae"))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:background "yellow")) (((class color) (min-colors 16) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 8)) (:foreground "black" :background "cyan")) (t (:inverse-video t))))
 '(trailing-whitespace ((t (:background "color-234"))))
 '(font-lock-builtin-face ((((class grayscale) (background light)) (:weight bold :foreground "LightGray")) (((class grayscale) (background dark)) (:weight bold :foreground "DimGray")) (((class color) (min-colors 88) (background light)) (:foreground "dark slate blue")) (((class color) (min-colors 88) (background dark)) (:foreground "LightSteelBlue")) (((class color) (min-colors 16) (background light)) (:foreground "Orchid")) (((class color) (min-colors 16) (background dark)) (:foreground "LightSteelBlue")) (((class color) (min-colors 8)) (:weight bold :foreground "blue")) (t (:weight bold))))
 '(font-lock-comment-delimiter-face ((t (:foreground "brightgreen"))))
 '(font-lock-constant-face ((((class grayscale) (background light)) (:underline t :weight bold :foreground "LightGray")) (((class grayscale) (background dark)) (:underline t :weight bold :foreground "Gray50")) (((class color) (min-colors 88) (background light)) (:foreground "dark cyan")) (((class color) (min-colors 88) (background dark)) (:foreground "Aquamarine")) (((class color) (min-colors 16) (background light)) (:foreground "CadetBlue")) (((class color) (min-colors 16) (background dark)) (:foreground "Aquamarine")) (((class color) (min-colors 8)) (:foreground "magenta")) (t (:underline t :weight bold))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face))))
 '(font-lock-function-name-face ((t (:background "black" :foreground "brightblue"))))
 '(font-lock-keyword-face ((((class grayscale) (background light)) (:weight bold :foreground "LightGray")) (((class grayscale) (background dark)) (:weight bold :foreground "DimGray")) (((class color) (min-colors 88) (background light)) (:foreground "Purple")) (((class color) (min-colors 88) (background dark)) (:foreground "Cyan1")) (((class color) (min-colors 16) (background light)) (:foreground "Purple")) (((class color) (min-colors 16) (background dark)) (:foreground "Cyan")) (((class color) (min-colors 8)) (:weight bold :foreground "cyan")) (t (:weight bold))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "color-237"))))
 '(font-lock-type-face ((((class grayscale) (background light)) (:weight bold :foreground "Gray90")) (((class grayscale) (background dark)) (:weight bold :foreground "DimGray")) (((class color) (min-colors 88) (background light)) (:foreground "ForestGreen")) (((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen")) (((class color) (min-colors 16) (background light)) (:foreground "ForestGreen")) (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen")) (((class color) (min-colors 8)) (:foreground "green")) (t (:underline t :weight bold))))
 '(font-lock-variable-name-face ((((class grayscale) (background light)) (:slant italic :weight bold :foreground "Gray90")) (((class grayscale) (background dark)) (:slant italic :weight bold :foreground "DimGray")) (((class color) (min-colors 88) (background light)) (:foreground "sienna")) (((class color) (min-colors 88) (background dark)) (:foreground "LightGoldenrod")) (((class color) (min-colors 16) (background light)) (:foreground "DarkGoldenrod")) (((class color) (min-colors 16) (background dark)) (:foreground "LightGoldenrod")) (((class color) (min-colors 8)) (:weight light :foreground "yellow")) (t (:slant italic :weight bold))))
 '(font-lock-warning-face ((t (:inherit (error)))))
 '(button ((t (:inherit (link)))))
 '(link ((((class color) (min-colors 88) (background light)) (:underline t :foreground "RoyalBlue3")) (((class color) (background light)) (:underline t :foreground "blue")) (((class color) (min-colors 88) (background dark)) (:underline t :foreground "cyan1")) (((class color) (background dark)) (:underline t :foreground "cyan")) (t (:inherit (underline)))))
 '(link-visited ((t (:foreground "magenta4" :inherit (link)))))
 '(fringe ((((class color) (background light)) (:background "grey95")) (((class color) (background dark)) (:background "grey10")) (t (:background "gray"))))
 '(header-line ((t (:inherit mode-line :background "gray20" :weight bold))))
 '(mode-line ((t (:background "#4271ae" :foreground "#eeeeec" :weight bold))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t nil)))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 1 :color "grey40" :style released-button) :foreground "brightgreen"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "color-233" :foreground "brightblue" :weight bold))))
 '(isearch ((t (:background "purple" :foreground "white"))))
 '(isearch-fail ((t (:weight ultra-bold :foreground "white" :background "red"))))
 '(lazy-highlight ((t (:background "black" :foreground "purple" :weight bold))))
 '(match ((t (:weight bold :foreground "brightgreen" :background "black"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(magit-item-highlight ((t (:background "color-233" :weight bold))))
 '(mode-line-read-only-face ((t (:inherit mode-line-face :foreground "#000000"))))
 '(mode-line-modified-face ((t (:inherit mode-line-face :background "#c82829" :foreground "#ffffff" :box (:line-width 2 :color "#ffffff") :weight bold))))
 '(mode-line-folder-face ((t (:inherit mode-line-face :foreground "black"))))
 '(mode-line-filename-face ((t (:inherit mode-line-face :foreground "white"))))
 '(mode-line-position-face ((t (:inherit mode-line-face))))
 '(mode-line-mode-face ((t (:inherit mode-line-face :foreground "color-233"))))
 '(mode-line-minor-mode-face ((t (:inherit mode-line-mode-face :foreground "bright black"))))
 '(mode-line-process-face ((t (:inherit mode-line-face :foreground "#718c00"))))
 '(mode-line-80col-face ((t (:background "yellow" :foreground "black"))))
 '(default ((t (:family "default" :foundry "default" :width normal :height 1 :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "unspecified-fg" :background "unspecified-bg" :stipple nil :inherit nil)))))

(provide-theme 'daemian)
