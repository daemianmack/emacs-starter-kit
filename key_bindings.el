
(defun explain-different-quit-keys ()
  (interactive)
  (message "Use C-x M-c to quit instead! It's harder to hit by accident."))

;; Rebind quit key and make it harder to hit. I rarely use it on purpose.
(global-set-key "\C-x\C-c" 'explain-different-quit-keys)
(global-set-key "\C-x\M-c" 'save-buffers-kill-emacs)

;; Override keystroke for query-replace. I almost always want the -regexp version instead.
(global-set-key (kbd "M-%") 'query-replace-regexp)

(global-set-key "\M-\C-y" 'kill-ring-search)

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
(global-set-key (kbd "C-x 4 r") 'util/rotate-windows)

;; I always hit space when I mean to jump.
(global-set-key (kbd "C-x r a")   'point-to-register) ; "assign" to point
(global-set-key (kbd "C-x r SPC") 'jump-to-register)  ; easy jump target

(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key (kbd "C-c =") 'easy-mark-sexp)

(global-set-key (kbd "C-c e r") 'eval-region)

(global-set-key (kbd "C-x C-y") 'util/pt-pbpaste)
(global-set-key (kbd "C-x M-w") 'util/pt-pbcopy)

(define-key emacs-lisp-mode-map (kbd "C-c v") 'eval-buffer)

(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c q") 'join-line)

(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)

(global-set-key (kbd "C-c C-a") 'align-cljlet)
 
(global-set-key (kbd "C-c C-s") 'clojure-toggle-keyword-string)

(global-set-key (kbd "C-w") 'util/backward-kill-word-or-kill-region)

(global-set-key (kbd "C-c w") 'whitespace-cleanup)

;; Better just-one-space.
(global-set-key (kbd "M-SPC") 'cycle-spacing)

(global-set-key (kbd "C-c +") 'util/increment-number-at-point)
(global-set-key (kbd "C-c -") 'util/decrement-number-at-point)

;; Enable escape sequences to trigger hyper-key combos.
;; Requires config in iTerm2:
;;  Preferences > Profiles > General > Keys > +
;;  `Keyboard Shortcut` triggers `Escape Sequence` of, e.g., [1;P9

(setq mac-command-modifier 'hyper)

(progn
  (define-key input-decode-map "\e[1;P9"  (kbd "H-a"))
  (define-key input-decode-map "\e[1;P10" (kbd "H-b"))
  (define-key input-decode-map "\e[1;P11" (kbd "H-c"))
  (define-key input-decode-map "\e[1;P12" (kbd "H-d"))
  (define-key input-decode-map "\e[1;P13" (kbd "H-e"))
  (define-key input-decode-map "\e[1;P14" (kbd "H-f"))
  (define-key input-decode-map "\e[1;P15" (kbd "H-g"))
  (define-key input-decode-map "\e[1;P16" (kbd "H-h"))
  (define-key input-decode-map "\e[1;P17" (kbd "H-i"))
  (define-key input-decode-map "\e[1;P18" (kbd "H-j"))
  (define-key input-decode-map "\e[1;P19" (kbd "H-k"))
  (define-key input-decode-map "\e[1;P20" (kbd "H-l"))
  (define-key input-decode-map "\e[1;P21" (kbd "H-m"))
  (define-key input-decode-map "\e[1;P22" (kbd "H-n"))
  (define-key input-decode-map "\e[1;P23" (kbd "H-o"))
  (define-key input-decode-map "\e[1;P24" (kbd "H-p"))
  (define-key input-decode-map "\e[1;P25" (kbd "H-q"))
  (define-key input-decode-map "\e[1;P26" (kbd "H-r"))
  (define-key input-decode-map "\e[1;P27" (kbd "H-s"))
  (define-key input-decode-map "\e[1;P28" (kbd "H-t"))
  (define-key input-decode-map "\e[1;P29" (kbd "H-u"))
  (define-key input-decode-map "\e[1;P30" (kbd "H-v"))
  (define-key input-decode-map "\e[1;P31" (kbd "H-w"))
  (define-key input-decode-map "\e[1;P32" (kbd "H-x"))
  (define-key input-decode-map "\e[1;P33" (kbd "H-y"))
  (define-key input-decode-map "\e[1;P34" (kbd "H-z"))
  (define-key input-decode-map "\e[1;P35" (kbd "H-0"))
  (define-key input-decode-map "\e[1;P36" (kbd "H-1"))
  (define-key input-decode-map "\e[1;P37" (kbd "H-2"))
  (define-key input-decode-map "\e[1;P38" (kbd "H-3"))
  (define-key input-decode-map "\e[1;P39" (kbd "H-4"))
  (define-key input-decode-map "\e[1;P40" (kbd "H-5"))
  (define-key input-decode-map "\e[1;P41" (kbd "H-6"))
  (define-key input-decode-map "\e[1;P42" (kbd "H-7"))
  (define-key input-decode-map "\e[1;P43" (kbd "H-8"))
  (define-key input-decode-map "\e[1;P44" (kbd "H-9"))

  ;; Creative escape sequences follow.
  ;;
  ;; iTerm2 sending \e[1;P44 will actually be consumed as \e[1;P4 with
  ;; a self-insert 4 following; disambiguate by avoiding any
  ;; single-digit-suffix that would so collide and pick
  ;; something else to tell iTerm2 to issue instead.

  (define-key input-decode-map "\e[1;Pa" (kbd "H-<right>"))
  (define-key input-decode-map "\e[1;Pb" (kbd "H-<left>"))
  (define-key input-decode-map "\e[1;Pc" (kbd "H-<up>"))
  (define-key input-decode-map "\e[1;Pd" (kbd "H-<down>"))

  (define-key input-decode-map "\e[1;p46" (kbd "H-."))
  (define-key input-decode-map "\e[1;P47" (kbd "H-["))
  (define-key input-decode-map "\e[1;P48" (kbd "H-]"))

  )
