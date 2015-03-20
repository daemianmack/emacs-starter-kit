
;; ;; tmux handles shift+arrow differently than screen. Accomodate. Prefer a fix in .tmux.conf.
;; (global-set-key (kbd "M-[ d") 'windmove-left)
;; (global-set-key (kbd "M-[ c") 'windmove-right)
;; (global-set-key (kbd "M-[ a") 'windmove-up)
;; (global-set-key (kbd "M-[ b") 'windmove-down)

(defun explain-different-quit-keys ()
  (interactive)
  (message "Use C-x M-c to quit instead! It's harder to hit by accident."))

;; Rebind quit key and make it harder to hit. I rarely use it on purpose.
(global-set-key "\C-x\C-c" 'explain-different-quit-keys)
(global-set-key "\C-x\M-c" 'save-buffers-kill-emacs)

;; I do, however, kill the hell out of some buffers.
;; If I add a C- to the second keystroke, kill without confirmation.
(defun my-kill-buffer ()
  "Just kill the current buffer without asking, unless it's a modified file"
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key "\C-x\C-k" 'my-kill-buffer)

;; Override keystroke for query-replace. I almost always want the -regexp version instead.
(global-set-key (kbd "M-%") 'query-replace-regexp)

(global-set-key (kbd "C-z") 'buffer-stack-down)
(global-set-key (kbd "M-C-z") 'buffer-stack-up)
(global-set-key [(f9)] 'buffer-stack-track)
(global-set-key [(control f9)] 'buffer-stack-untrack)
(global-set-key [(f12)] 'buffer-stack-bury)
(global-set-key [(control f12)] 'buffer-stack-bury-and-kill)

(global-set-key "\M-\C-y" 'kill-ring-search)

(global-set-key (kbd "C-c ,") 'tags-search)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'execute-extended-command)
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

;; Use standard navigation keys that make sense vertically
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map [down] 'ido-next-match)
            (define-key ido-completion-map [up] 'ido-prev-match)
            (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
            (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)))

(global-set-key (kbd "C-c %") 'goto-match-paren)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c C-g") 'rgrep)
(global-set-key (kbd "C-c ^") 'query-replace-regexp)
(global-set-key (kbd "C-x 4 r") 'rotate-windows)

;; I always hit space when I mean to jump.
(global-set-key (kbd "C-x r a")   'point-to-register) ; "assign" to point
(global-set-key (kbd "C-x r SPC") 'jump-to-register)  ; easy jump target


(key-chord-define-global "jk" 'ace-jump-word-mode)
(key-chord-define-global ",." 'other-window)
(key-chord-define-global "zz" 'save-buffer)
(key-chord-define-global "zx" 'beginning-of-buffer)


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

(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key (kbd "C-c =") 'easy-mark-sexp)

(global-set-key (kbd "C-c e r") 'eval-region)

(global-set-key (kbd "C-c h t") 'highlight-this)

; Integrate with tmux splits.
(global-set-key (kbd "S-<up>")
                '(lambda () (interactive) (windmove-emacs-or-tmux "up"  "tmux select-pane -U")))
(global-set-key (kbd "S-<down>")
                '(lambda () (interactive) (windmove-emacs-or-tmux "down"  "tmux select-pane -D")))
(global-set-key (kbd "S-<right>")
                '(lambda () (interactive) (windmove-emacs-or-tmux "right" "tmux select-pane -R")))
(global-set-key (kbd "S-<left>")
                '(lambda () (interactive) (windmove-emacs-or-tmux "left"  "tmux select-pane -L")))

(cljr-add-keybindings-with-prefix "C-c C-f")

(global-set-key (kbd "C-x C-y") 'pt-pbpaste)
(global-set-key (kbd "C-x M-w") 'pt-pbcopy)

(define-key emacs-lisp-mode-map (kbd "C-c v") 'eval-buffer)

(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c q") 'join-line)

(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)

(global-set-key (kbd "C-c <C-right>") 'org-do-demote)
(global-set-key (kbd "C-c <C-left>") 'org-do-promote)

(global-set-key (kbd "C-c C-a") 'align-cljlet)
 
(global-set-key (kbd "C-c C-s") 'clojure-toggle-keyword-string)
(global-set-key (kbd "C-c C-q") 'cider-restart)

;; Let existence of an active region determine whether we backward-kill-word, or kill said region.
(defun backward-kill-word-or-kill-region (&optional arg)
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))
(global-set-key (kbd "C-w") 'backward-kill-word-or-kill-region)

(global-set-key (kbd "C-c w") 'whitespace-cleanup)
