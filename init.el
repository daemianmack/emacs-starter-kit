;; Put timestamps in *Messages* buffer to diag start-time slowness.
(defun current-time-microseconds ()
  (let* ((nowtime (current-time))
         (now-ms (nth 2 nowtime)))
    (concat (format-time-string "[%Y-%m-%dT%T" nowtime) (format ".%d] " now-ms))))

(defadvice message (before test-symbol activate)
  (if (not (string-equal (ad-get-arg 0) "%s%s"))
      (let ((deactivate-mark nil)
            (inhibit-read-only t))
        (save-excursion
          (set-buffer "*Messages*")
          (goto-char (point-max))
          (if (not (bolp))
              (newline))
                    (insert (current-time-microseconds))))))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Add in your own as you wish:
(defvar my-packages '(
                      browse-kill-ring
                      buffer-stack
                      cider
                      clj-refactor
                      clojure-mode
                      clojure-test-mode
                      easy-kill
                      elisp-slime-nav
                      find-file-in-project
                      flymake-cursor
                      git-messenger
                      hlinum
                      idle-highlight-mode
                      ido-ubiquitous
                      ipython
                      key-chord
                      kill-ring-search
                      magit
                      nav
                      projectile
                      project-explorer
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
