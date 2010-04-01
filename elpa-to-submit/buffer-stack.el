;;; buffer-stack.el --- Smart movement through the buffer list.

;; Copyright (C) 2002 Adrian Kubala

;; Author: Adrian Kubala <adrian@sixfingeredman.net>
;; URL: <http://www.sixfingeredman.net/proj/xemacs>
;; Created: Thu 13 Jun 02
;; Version: 
;; Keywords: buffer, buffers, switching buffers

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary
;;
;; This is tested with XEmacs 21.4 and GNU Emacs 21 (FSFmacs).
;;
;; If you have used "Alt-Tab" in Windows, you know the basic principle
;; behind this buffer model. Your buffers are in a stack, with the
;; most recent on top. When you want a new buffer, you scan through
;; them until you find the one you want, and then it goes on top of
;; the stack.
;;
;; The stack may be filtered either inclusively or exclusively. It may
;; be global or frame-specific. The default is frame-specific and
;; exclusive. For more details see the code docstrings.
;;
;; To use, put this file in your library path. In your .emacs or
;; init.el, put:
;;
;; (require 'buffer-stack)
;;
;; I use these key-bindings:
;;
;; (global-set-key [(f9)] 'buffer-stack-track)
;; (global-set-key [(control f9)] 'buffer-stack-untrack)
;; (global-set-key [(f10)] 'buffer-stack-up)
;; (global-set-key [(f11)] 'buffer-stack-down)
;; (global-set-key [(f12)] 'buffer-stack-bury)
;; (global-set-key [(control f12)] 'buffer-stack-bury-and-kill)
;;
;; If you want to preserve your settings between sessions (i.e. 
;; tracked buffers), save customization settings on exit or use
;; desktop's save-global feature.
;;
;; This is similar to some other libraries which I didn't know about when I
;; first wrote it. See also http://www.emacswiki.org/cgi-bin/wiki.pl?SwitchingBuffers
;;      ibs.el -- http://www.geekware.de/software/emacs/#ibs
;;      bubble-buffer.el -- http://www.glue.umd.edu/~deego/emacspub/lisp-mine/bubble/
;;	pc-bufsw -- http://asfys3.fi.uib.no/~boukanov/emacs/index.html
;;
;; Thanks to gerds@paracelsus.fdm.uni-freiburg.de for feedback and
;; requesting the inclusive stack, deego@glue.umd.edu for feedback,
;; and LathI for requesting frame-specific stacks.

;;; Bugs
;;
;; The switch from frame-specific to global in FSFmacs might not work
;; correctly.
;;
;; Untracked buffers may get rearranged relative to tracked ones.
;;
;; It is assumed that buffers don't rearrange themselves. That is, if
;; you're in the middle of switching and the buffer-list changes
;; spontaneously, things could become totally screwed up.
;;
;; You need to use the buffer-stack-* commands directly with
;; keybindings; `M-x buffer-stack-*' will not work, because this
;; changes `last-command'.


;;; ChangeLog

;; 1.2 2002-06-27
;;      Workaround for broken FSFmacs bury-buffer.

;; 1.1 2002-06-23
;;      New implementation which uses buffer-list directly. Dropping
;;      frame-specific-tracking, mountain stack. Works with FSFmacs!

;; 1.0 2002-06-23
;;      Sealing corner cases and fixing bugs in frame-specific usage.

;; 0.9 2002-06-18
;;      Public release. Includes customization variables.

;; 0.3--0.4b 2002-06-16
;;      First attempts at frame-specific and inclusive-mode features.

;; 0.1--0.2 2002-06-13
;;      Initial release and minor correction. Basic mountain stack.

;;; Code:

(provide 'buffer-stack)

;;; public variables / configuration

(defgroup buffer-stack nil
  "Smart movement through the buffer list."
  :group 'editing
  :prefix "buffer-stack-"
  :link '(emacs-commentary-link :tag "Commentary" "buffer-stack.el")
  :link '(emacs-library-link :tag "Lisp File" "buffer-stack.el"))

(defcustom buffer-stack-show-position t
  "Show position in the stack when moving?
If true, a message like 'buffer-stack: n' will be printed after each
movement through the stack, where n is your current position in the
stack."
  :type 'boolean
  :group 'buffer-stack)

(defcustom buffer-stack-frame-specific t
  "Does each frame maintain a seperate buffer stack?
Broken in GNU Emacs."
  :type 'boolean
  :group 'buffer-stack)

(defcustom buffer-stack-exclusive t
  "Do we use exclusive filtering?
If true, only explicitly untracked buffers are hidden. If false, only
explicitly tracked buffers are shown (inclusive filtering)."
  :type 'boolean
  :group 'buffer-stack)

(defcustom buffer-stack-untracked
  '("KILL" "*Compile-Log*" "*Ibuffer*" "*Compile-Log-Show*" "*Group*" "*Completions*"
    "*Kill Ring*" "*Messages*")
  "The list of buffer names to hide when switching through the stack.
This is used only when `buffer-stack-exclusive' is true, and is in
addition to permanently-hidden buffers which start with a space."
  :type '(repeat string)
  :group 'buffer-stack)

(defcustom buffer-stack-tracked nil
  "The list of buffer names to show when switching through the stack.
This is used only when `buffer-stack-exclusive' is false."
  :type '(repeat string)
  :group 'buffer-stack)

;;; private variables

;; these all only have meaning during switching

(defvar buffer-stack-last-frame nil
  "The frame we're moving in.")

(defvar buffer-stack-index nil
  "Our position in the stack.")

(defvar buffer-stack nil
  "Stack of buffers in order, from most recent to least.")

;;; interactive functions

;;;###autoload
(defun buffer-stack-down ()
  "Move down in the buffer stack."
  (interactive)
  (buffer-stack-move 1)
  (buffer-stack-show-position))

;;;###autoload
(defun buffer-stack-up ()
  "Move up in the buffer stack."
  (interactive)
  (buffer-stack-move -1)
  (buffer-stack-show-position))

;;;###autoload
(defun buffer-stack-bury-and-kill ()
  "Bury the current buffer, then kill it."
  (interactive)
  (let ((buffer (current-buffer)))
    ;; this relies on the fact that buffer-stack-bury never leaves us
    ;; on the same buffer.
    (condition-case ()
        (buffer-stack-bury)
      ;; the stack was empty, but we want to kill the buffer anyways
      (error nil))
    (kill-buffer buffer)
    (setq buffer-stack (buffer-stack-clean buffer-stack))))

;;;###autoload
(defun buffer-stack-track ()
  "Track the current buffer.
In inclusive mode, this means add it to the tracked list. Otherwise,
remove it from the untracked list."
  (interactive)
  (if buffer-stack-exclusive
      (setq buffer-stack-untracked (delete (buffer-name (current-buffer)) buffer-stack-untracked))
    (add-to-list 'buffer-stack-tracked (buffer-name (current-buffer)))))

;;;###autoload
(defun buffer-stack-untrack ()
  "Untrack the current buffer.
In inclusive mode, this means remove it from the tracked list. 
Otherwise, add it to the untracked list."
  (interactive)
  (if buffer-stack-exclusive
      (add-to-list 'buffer-stack-untracked (buffer-name (current-buffer)))
    (setq buffer-stack-tracked (delete (buffer-name (current-buffer)) buffer-stack-tracked))))

;;;###autoload
(defun buffer-stack-bury ()
  "Bury the current buffer and move to the next in the stack."
  (interactive)
  (if (and (buffer-stack-switching-p) (>= buffer-stack-index (- (length buffer-stack) 1)))
      (progn
        (beep)
        (message "Tried to bury bottom-most buffer!")
        (buffer-stack-move -1))
    (let ((buffer (current-buffer)))
      (when (buffer-stack-switching-p)
        (when (null buffer-stack)
          ;; we must not be tracking anything
          (error "There are no buffers in the stack!"))
        ;; send to bottom of stack
        (setq buffer-stack (delq buffer buffer-stack))
        (when (buffer-stack-tracked-p buffer)
          (setq buffer-stack (nconc buffer-stack (list buffer)))))
      ;; bury in the real list
      (buffer-stack-bury-buffer buffer)
      ;; pull the "next" buffer to the top
      (buffer-stack-move 0)
      (buffer-stack-show-position))))

;;; private functions

;; This is the most important function. The principle is, if we're
;; starting a switch, first build a list of buffers to switch between. 
;; If we're doing a switch, use this list to find the buffer we're
;; supposed to jump to, and bubble it with the current buffer.
(defun buffer-stack-move (direction)
  "Move through the stack by one buffer.
This is THE switching command; all other motions are based on this."
  (setq this-command 'buffer-stack-move)
  (unless (buffer-stack-switching-p)
    ;; prepare a stack
    (setq buffer-stack-index 0)
    (buffer-stack-rebuild)
    (setq buffer-stack-last-frame (selected-frame))
    (unless (buffer-stack-tracked-p (current-buffer))
      (setq buffer-stack (cons (current-buffer) buffer-stack))))
  ;; This relies on the fact that the buffer list doesn't change
  ;; during switching.
  (when (null buffer-stack)
    ;; we must not be tracking anything
    (error "There are no buffers in the stack!"))
  (let (buffer
        (max-index (- (length buffer-stack) 1)))
    ;; find the new index
    (if (> direction 0)
        (incf buffer-stack-index)
      (if (< direction 0)
          (decf buffer-stack-index)))
    (if (< buffer-stack-index 0)
        ;; go backwards to the last buffer
        (progn (setq buffer-stack-index max-index)
               ;; this works in FSFmacs!
               (switch-to-buffer (nth buffer-stack-index buffer-stack))
               (beep))
      (if (> buffer-stack-index max-index)
          ;; wrap to the first buffer
          (progn (setq buffer-stack-index 0)
                 (buffer-stack-bury-buffer (current-buffer))
                 (switch-to-buffer (first buffer-stack))
                 (beep))
        ;; the usual case, we put the top buffer before the indexed
        ;; buffer and the indexed buffer on top
        (setq buffer (nth buffer-stack-index buffer-stack))
        (buffer-stack-bury-buffer (current-buffer) buffer)
        (switch-to-buffer buffer)
        ))))

(defun buffer-stack-bury-buffer (buffer &optional before)
  "Emulate xemacs's bury-buffer for fsfmacs."
  (if (featurep 'xemacs) (bury-buffer buffer before)
    (if buffer-stack-frame-specific
        (let* ((frame (selected-frame))
               (new-list (buffer-list frame))
               (rest new-list))
          (setq new-list (delq buffer new-list))
          (if (null new-list)
              (setq new-list (list buffer))
            (if (null before)
                (setq new-list (nconc new-list (list buffer)))
              (if (eq before (car new-list))
                  (setq new-list (cons buffer new-list))
                (while (not (or (null (cdr rest)) (eq (cadr rest) before)))
                  (setq rest (cdr rest)))
                (setcdr rest (cons buffer (cdr rest)))
                )))
          (modify-frame-parameters frame (list (cons 'buffer-list new-list))))
      ;; this didn't seem to operate on the correct frame for the
      ;; frame-specific case
      (dolist (b (buffer-list nil))
        (when (eq b before)
          (bury-buffer buffer))
        (unless (eq b buffer)
          (bury-buffer b))))))

(defun buffer-stack-rebuild ()
  "Create `buffer-stack' from the buffer list."
  (setq buffer-stack (buffer-stack-clean (buffer-list (buffer-stack-frame)))))

(defun buffer-stack-clean (buffer-list)
  "Remove untracked buffers from a list by side effect."
  (let ((rest buffer-list)
	buffer
        last
	new-stack)
    (while (not (null rest))
      (setq buffer (car rest))
      (if (buffer-stack-tracked-p buffer)
          (setq last rest)
        (if last
            (setcdr last (cdr rest))
          (setq buffer-list (cdr rest))))
      (setq rest (cdr rest)))
    buffer-list))

(defun buffer-stack-switching-p ()
  "Are we switching buffers?"
  (and (eq last-command 'buffer-stack-move) (eq buffer-stack-last-frame (selected-frame))))

(defun buffer-stack-show-position ()
  "Print the current position."
  (if buffer-stack-show-position
      (message (concat "buffer-stack: " (prin1-to-string (abs buffer-stack-index))))))

(defun buffer-stack-tracked-p (buffer)
  "Is this buffer tracked?"
  (let ((name (buffer-name buffer)))
    (if buffer-stack-exclusive
        (not (or (null name)
                 (char-equal ?  (string-to-char name))
                 (member name buffer-stack-untracked)))
      (member name buffer-stack-tracked)
      )))

(defun buffer-stack-frame ()
  (if buffer-stack-frame-specific
      (selected-frame)
    (if (featurep 'xemacs)
        t
      nil)))

;;; buffer-stack.el ends here
