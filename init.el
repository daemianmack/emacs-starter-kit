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
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(package-initialize)

(defvar my-packages '(ac-cider
                      align-cljlet
                      browse-kill-ring
                      buffer-stack
                      cider
                      clj-refactor
                      clojure-mode
                      clojure-snippets
                      easy-kill
                      find-file-in-project
                      flymake-cursor
                      helm
                      hlinum
		      highlight-symbol
                      ido-ubiquitous
                      ido-completing-read+ ;; For magit.
                      key-chord
                      kill-ring-search
		      kpm-list
                      magit
                      nav
                      projectile
                      project-explorer
                      python-mode
                      rainbow-delimiters
                      rings
                      smex
                      smooth-scrolling
                      undo-tree
                      yasnippet)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
        (package-install p)))
(put 'downcase-region 'disabled nil)


(setq dotfiles-dir (file-name-directory
		    (or (buffer-file-name) load-file-name)))
(setq user-config (concat dotfiles-dir user-login-name ".el"))
(when (file-exists-p user-config)
  (load user-config))
