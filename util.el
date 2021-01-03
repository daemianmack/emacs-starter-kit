(defun util/is-in-terminal () (not (display-graphic-p)))

(defun smarter-backward-kill-word ()
  "Deletes the previous word, respecting:
1. If the cursor is at the beginning of line, delete the '\n'.
2. If there is only whitespace, delete only to beginning of line.
3. If there is whitespace, delete whitespace and check 4-5.
4. If there are other characters instead of words, delete one only char.
5. If it's a word at point, delete it."
  (interactive)

  (if (bolp)
      ;; 1
      (delete-char -1)

    (if (string-match-p "^[[:space:]]+$"
                        (buffer-substring-no-properties
                         (line-beginning-position) (point)))
        ;; 2
        (delete-horizontal-space)

      (when (thing-at-point 'whitespace)
        ;; 3
        (delete-horizontal-space))

      (if (thing-at-point 'word)
          ;; 5
          (let ((start (car (bounds-of-thing-at-point 'word)))
                (end (cdr (bounds-of-thing-at-point 'word))))
            (if (> end start)
                (delete-region start end)
              (delete-char -1)))
        ;; 4
        (delete-char -1)))))

(defun util/backward-kill-word-or-kill-region (&optional arg)
  "Let existence of an active region determine whether we `backward-kill-word`, or kill said region."
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (smarter-backward-kill-word)))


;; Flips the left and right windows. Taken from
;; http://whattheemacsd.com//buffer-defuns.el-02.html
(defun util/rotate-windows ()
  "Rotate your windows."
  (interactive)
  (cond ((not (> (count-windows) 1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2)))
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defvar former-window-configuration nil
  "Stores previous window configurations, e.g. those that were in effect when center-window-horizontally was called.")

(defun util/center-window-horizontally (width)
  "Arrange windows three as side-by-side, with the center one
having width WIDTH. Accepts WIDTH as a numeric prefix, but
defaults to 85."
  (interactive "P")
  (push (current-window-configuration) former-window-configuration)
  (let ((width (or width 85)))
    (let ((side-window-width (/ (- (frame-parameter nil 'width) width) 2)))
      (delete-other-windows)
      (set-window-buffer (split-window-horizontally side-window-width)
                         (other-buffer nil nil))
      (other-window 1)
      (set-window-buffer (split-window-horizontally (- side-window-width))
                         (other-buffer nil nil)))))


(defun util/windmove-emacs-or-tmux (dir tmux-dir)
  "Allow movement between split windows or into tmux panes sharing the tmux screen."
  (interactive)
  (if (ignore-errors (funcall (intern (concat "windmove-" dir))))
      nil                     ;; Moving within emacs
    (shell-command (concat "tmux select-pane " tmux-dir))) ;; At edges, drive tmux
  )

(defun util/pt-pbpaste ()
  "Paste data from pasteboard."
  (interactive)
  (shell-command-on-region
   (point)
   (if mark-active (mark) (point))
   "pbpaste" nil t))

(defun util/pt-pbcopy ()
  "Copy region to pasteboard."
  (interactive)
  (print (mark))
  (when mark-active
    (shell-command-on-region
     (point) (mark) "pbcopy")
    (kill-buffer "*Shell Command Output*")))


(defun util/open-in-external-app ()
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2016-10-15"
  (interactive)
  (let* (
         (-file-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         (-do-it-p (if (<= (length -file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when -do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda (-fpath)
           (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" -fpath t t))) -file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda (-fpath)
           (shell-command
            (concat "open " (shell-quote-argument -fpath))))  -file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda (-fpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" -fpath))) -file-list))))))

(defun util/open-in-desktop ()
  "Show current file in desktop (OS's file manager).
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-11-30"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((string-equal system-type "darwin") (shell-command "open ."))
   ((string-equal system-type "gnu/linux")
    (let (
          (process-connection-type nil)
          (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
                               "/usr/bin/gvfs-open"
                             "/usr/bin/xdg-open")))
      (start-process "" nil openFileProgram "."))
    ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. For example: with nautilus
    )))

(defun util/delete-register (name)
  (setq register-alist
        (delq (assoc name register-alist)
              register-alist)))

(defun util/advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(defun util/crux-rename-file-and-buffer ()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let* ((new-name (read-from-minibuffer "New name: " filename))
             (containing-dir (file-name-directory new-name)))
        (make-directory containing-dir t)
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun util/crux-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename delete-by-moving-to-trash)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defvar util/mode-line-buffer-line-count nil)
(make-variable-buffer-local 'util/mode-line-buffer-line-count)

(defun util/mode-line-count-lines ()
  (validate-setq util/mode-line-buffer-line-count
                 (int-to-string (count-lines (point-min) (point-max)))))

(defun util/add-watchwords ()
  (interactive)
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(defun util/comment-auto-fill ()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))

(defun util/lisp-mode-setup ()
  (eldoc-mode)
  (paredit-mode 1)
  (whitespace-mode)
  (rainbow-delimiters-mode))

(defun util/no-final-newline ()
  (validate-setq require-final-newline nil)
  (validate-setq mode-require-final-newline nil))

(defun util/paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(defun util/copy-file-path (&optional @dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.

Result is full path.

If `universal-argument' is called first, copy only the dir path.

If a buffer is not file and not dired, copy value of
`default-directory' (which is usually the “current” dir when that
buffer was created)

Modified from
`http://ergoemacs.org/emacs/emacs_copy_file_path.html' to ignore
dired, which I don't use."
  (interactive "P")
  (let (($fpath
         (if (buffer-file-name)
             (buffer-file-name)
           (expand-file-name default-directory))))
    (util/paste-to-osx
     (if @dir-path-only-p
         (progn
           (message "Directory path copied: 「%s」" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: 「%s」" $fpath)
         $fpath )))))

(defun util/clojure-copy-ns ()
  "Update the namespace of the current buffer.
    Useful if a file has been renamed."
  (interactive)
  (let ((nsname (funcall clojure-expected-ns-function)))
    (when nsname
      (message nsname)
      (kill-new nsname)
      (util/paste-to-osx nsname))))

(defun util/swoop-top-level-forms ()
  (interactive)
  (swoop "^("))

(defun util/ido-disable-line-truncation ()
  (set (make-local-variable 'truncate-lines) nil))

;; TODO
;; - Use var for save location.
;; - Use a filter so we don't trigger hook fn when editing some repo's README.org.

(defun util/git-log-changes-hook()
  "Record a diff in git every time we save in org mode. This
   gives us an edit history without having to org-time-stamp
   everything."
  (interactive)
  ;; TODO This should accept the filename from the buffer object or similar
  ;; instead of hardcoding a single known file.
  (shell-command "cd ~/Dropbox/docs/notes/ && \
                  git add notes.org && \
                  git commit -m \"$(git diff --cached --unified=0 | tail -n +6)\""))



(defun util/increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun util/decrement-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1- (string-to-number (match-string 0))))))

(defun util/round-to-nearest-percentage (n m)
  "Round N to nearest multiple of float M."
  (let ((mlt (/ 1 m)))
    (/ (fround (* n mlt))
       mlt)))

(defun util/finder ()
  "Open the current working directory in finder."
  (interactive)
  (shell-command (concat "open " (shell-quote-argument default-directory))))

(defun util/kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(defun util/clean-buffer-list-now ()
  (interactive)
  (let ((orig (eval clean-buffer-list-delay-special)))
    (validate-setq clean-buffer-list-delay-special 0)
    (clean-buffer-list)
    (validate-setq clean-buffer-list-delay-special orig)))

(defun util/gh-visit-pr ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\(\\.git\\)?\\'" "\\1"
            (magit-get "remote"
                       "origin"
                       "url"))
           (magit-get-current-branch))))

(defun set-selective-display-dlw (&optional level)
  "Fold text indented same of more than the cursor.
If level is set, set the indent level to LEVEL.
If 'selective-display' is already set to LEVEL, clicking
F5 again will unset 'selective-display' by setting it to 0."
  (interactive "P")
  (if (eq selective-display (1+ (current-column)))
      (set-selective-display 0)
    (set-selective-display (or level (1+ (current-column))))))

;;; defuns
(defvar util/selective-display-width 1
  "Last non nil value of `selective-display'.")

(defun selective-display--incf (offset)
  "Increments `selective-display' by OFFSET."
  (setq util/selective-display-width (+ util/selective-display-width offset))
  (set-selective-display util/selective-display-width))

(defun util/selective-display-increase ()
  "Increase the cap for `selective-display'."
  (interactive)
  (when (< util/selective-display-width 20)
    (selective-display--incf 2)))

(defun util/selective-display-decrease ()
  "Decrease the cap for `selective-display'."
  (interactive)
  (when (> util/selective-display-width 1)
    (selective-display--incf -2)))

(defun util/toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))

(defconst util/do-not-kill-buffer-names '("*scratch*" "*Messages*")
  "Names of buffers that should not be killed.")

(defun util/do-not-kill-important-buffers ()
  "Inhibit killing of important buffers.
Add this to `kill-buffer-query-functions'."
  (if (not (member (buffer-name) util/do-not-kill-buffer-names))
      t
    (message "Not allowed to kill %s, burying instead" (buffer-name))
    (bury-buffer)
    nil))

(defun util/my-kill-buffer ()
  "Just kill the current buffer without asking, unless it's a modified file"
  (interactive)
  (kill-buffer (current-buffer)))
