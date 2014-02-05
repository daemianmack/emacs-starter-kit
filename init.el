(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Add in your own as you wish:
(defvar my-packages '(
                      ag
                      browse-kill-ring
                      buffer-stack
                      cider
                      clojure-mode
                      clojure-test-mode
                      easy-kill
                      elisp-slime-nav
                      find-file-in-project
                      flymake-cursor
                      hlinum
                      idle-highlight-mode
                      ido-ubiquitous
                      ipython
                      key-chord
                      kill-ring-search
                      magit
                      nav
                      projectile
                      python-mode
                      rainbow-delimiters
                      rings
                      smex
                      smooth-scrolling
                      starter-kit
                      starter-kit-bindings
                      starter-kit-lisp
                      starter-kit-ruby
                      undo-tree
                      )

  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
        (package-install p)))
(put 'downcase-region 'disabled nil)
