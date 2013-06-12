(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Add in your own as you wish:
(defvar my-packages '(magit starter-kit starter-kit-lisp starter-kit-bindings starter-kit-ruby clojure-mode ipython python-mode smooth-scrolling nav buffer-stack kill-ring-search browse-kill-ring flymake-cursor rainbow-delimiters hlinum nrepl nrepl-ritz idle-highlight-mode find-file-in-project smex ido-ubiquitous elisp-slime-nav projectile key-chord undo-tree)

  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
        (package-install p)))
(put 'downcase-region 'disabled nil)
