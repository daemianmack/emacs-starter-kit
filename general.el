;; vi-style %
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(defun reset-highlight ()
  (interactive)
  (global-hi-lock-mode 0)
  (global-hi-lock-mode 1))

(defun highlight-this ()
  (interactive)
  (highlight-regexp (regexp-quote (word-at-point))))


(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
  (message "Ah, much better!"))


;; Flips the left and right windows. Taken from
;; http://whattheemacsd.com//buffer-defuns.el-02.html
(defun rotate-windows ()
  "Rotate your windows"
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

(defun center-window-horizontally (width)
  "Arrange windows three as side-by-side, with the center one
having width WIDTH.
Accepts WIDTH as a numeric prefix, but defaults to 85."
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


(defun windmove-emacs-or-tmux (dir tmux-cmd)
  (interactive)
  (if (ignore-errors (funcall (intern (concat "windmove-" dir))))
      nil                     ;; Moving within emacs
    (shell-command tmux-cmd)) ;; At edges, send command to tmux
  )

(defun write-region-to-tmux-buffer (beg end)
  (interactive "r")
  (shell-command-on-region beg end "tmux load-buffer -" nil nil nil t))

(defun write-buffer-to-tmux-buffer ()
  (interactive)
    (write-region-to-tmux-buffer (point-min) (point-max)))


(defun pt-pbpaste ()
  "Paste data from pasteboard."
  (interactive)
  (shell-command-on-region
   (point)
   (if mark-active (mark) (point))
   "pbpaste" nil t))

(defun pt-pbcopy ()
  "Copy region to pasteboard."
  (interactive)
  (print (mark))
  (when mark-active
    (shell-command-on-region
     (point) (mark) "pbcopy")
    (kill-buffer "*Shell Command Output*")))


(defun cider-repl-reset ()
  (interactive)
  (save-some-buffers)
  (with-current-buffer (cider-current-repl-buffer)
    (goto-char (point-max))
    (insert "(dev/reset)")
    (cider-repl-return)))

(global-set-key (kbd "C-c !") 'cider-repl-reset)

(defun reload-repl ()
  (interactive)
  (cider-quit)
  (cider-jack-in))

(defun cider-repl-command (cmd)
  "Execute commands on the cider repl"
  (cider-switch-to-repl-buffer)
  (goto-char (point-max))
  (insert cmd)
  (cider-repl-return)
  (cider-switch-to-last-clojure-buffer))

(defun cider-reset-repl ()
  "Assumes reloaded + tools.namespace is used to reload everything"
  (interactive)
  (save-some-buffers)
  (cider-repl-command "(repl/reload)"))

(defun cider-reset-repl-run-tests ()
  (interactive)
  (cider-reset-repl)
  (clojure-test-run-tests)) ;; Upgrade CIDER some day and use its cider-test-run-tests?


;; cider-mode-map is only available in future version of cider.
;; (define-key cider-mode-map (kbd "C-c !") 'cider-reset-repl)
;; (define-key cider-mode-map (kbd "C-c .") 'cider-reset-repl-run-tests)


;; inferior lisp mode stuff
;; (setq inferior-lisp-program "lein repl")
;; (add-hook 'clojure-mode-hook
;;           '(lambda ()
;;              (define-key clojure-mode-map
;;                "\C-c\C-k"
;;                '(lambda ()
;;                   (interactive)
;;                   (let ((current-point (point)))
;;                     (goto-char (point-min))
;;                     (let ((ns-idx (re-search-forward clojure-namespace-name-regex nil t)))
;;                       (when ns-idx
;;                         (goto-char ns-idx)
;;                         (let ((sym (symbol-at-point)))
;;                           (message (format "Loading %s ..." sym))
;;                           (lisp-eval-string (format "(require '%s :reload)" sym))
;;                           (lisp-eval-string (format "(in-ns '%s)" sym)))))
;;                     (goto-char current-point))))))
 
;; (add-hook 'inferior-lisp-mode-hook
;;           '(lambda ()
;;              (define-key inferior-lisp-mode-map
;;                "\C-cl"
;;                '(lambda ()
;;                   (interactive)
;;                   (erase-buffer)
;;                   (lisp-eval-string "")))))
