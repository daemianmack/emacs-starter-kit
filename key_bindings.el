
(defun explain-different-quit-keys ()
  (interactive)
  (message "Use C-x M-c to quit instead! It's harder to hit by accident."))

;; Rebind quit key and make it harder to hit. I rarely use it on purpose.
(global-set-key "\C-x\C-c" 'explain-different-quit-keys)
(global-set-key "\C-x\M-c" 'save-buffers-kill-emacs)

;; Override keystroke for query-replace. I almost always want the -regexp version instead.
(global-set-key (kbd "M-%") 'query-replace-regexp)

(global-set-key "\M-\C-y" 'kill-ring-search)

(global-set-key (kbd "C-c ,") 'tags-search)
(global-set-key (kbd "C-c m") 'back-to-operand)

;; Make shift-up work.
(define-key input-decode-map "\e[1;2A" [S-up])
;; The above makes this obsolete, or?
;; Make shift-up work instead of triggering "<select> is undefined".
(defadvice terminal-init-xterm (after select-shift-up activate)
      (define-key input-decode-map "\e[1;2A" [S-up]))


;; Unnecessary for shift-up at least from a Darwin console.
;; (if (equal "xterm" (tty-type))
;;     (define-key input-decode-map "\e[1;2A" [S-up]))
;; (defadvice terminal-init-xterm (after select-shift-up activate)
;;       (define-key input-decode-map "\e[1;2A" [S-up]))

(global-set-key (kbd "C-c %") 'goto-match-paren)
(global-set-key (kbd "C-c C-g") 'rgrep)
(global-set-key (kbd "C-c ^") 'query-replace-regexp)
(global-set-key (kbd "C-x 4 r") 'rotate-windows)

;; I always hit space when I mean to jump.
(global-set-key (kbd "C-x r a")   'point-to-register) ; "assign" to point
(global-set-key (kbd "C-x r SPC") 'jump-to-register)  ; easy jump target

(global-set-key (kbd "<f1>")   (rings-generate-cycler 1))
(global-set-key (kbd "C-c <f1>") (rings-generate-setter 1))
(global-set-key (kbd "<f2>")   (rings-generate-cycler 2))
(global-set-key (kbd "C-c <f2>") (rings-generate-setter 2))
(global-set-key (kbd "<f3>")   (rings-generate-cycler 3))
(global-set-key (kbd "C-c <f3>") (rings-generate-setter 3))
(global-set-key (kbd "<f4>")   (rings-generate-cycler 4))
(global-set-key (kbd "C-c <f4>") (rings-generate-setter 4))
(global-set-key (kbd "<f5>")   (rings-generate-cycler 5))
(global-set-key (kbd "C-c <f5>") (rings-generate-setter 5))
(global-set-key (kbd "<f6>")   (rings-generate-cycler 6))
(global-set-key (kbd "C-c <f6>") (rings-generate-setter 6))
(global-set-key (kbd "<f11>")   (rings-generate-cycler 11))
(global-set-key (kbd "C-c <f11>") (rings-generate-setter 11))

(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key (kbd "C-c =") 'easy-mark-sexp)

(global-set-key (kbd "C-c e r") 'eval-region)

(global-set-key (kbd "C-c h t") 'highlight-this)

(global-set-key (kbd "C-x C-y") 'pt-pbpaste)
(global-set-key (kbd "C-x M-w") 'pt-pbcopy)

(define-key emacs-lisp-mode-map (kbd "C-c v") 'eval-buffer)

(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c q") 'join-line)

(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)

(global-set-key (kbd "C-c C-a") 'align-cljlet)
 
(global-set-key (kbd "C-c C-s") 'clojure-toggle-keyword-string)

;; Let existence of an active region determine whether we backward-kill-word, or kill said region.
(defun backward-kill-word-or-kill-region (&optional arg)
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))
(global-set-key (kbd "C-w") 'backward-kill-word-or-kill-region)

(global-set-key (kbd "C-c w") 'whitespace-cleanup)

;; Better just-one-space.
(global-set-key (kbd "M-SPC") 'cycle-spacing)
