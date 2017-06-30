(setq dotfiles-dir (file-name-directory
                     (or (buffer-file-name) load-file-name)))

(let ((default-directory (concat dotfiles-dir "elpa")))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (normal-top-level-add-subdirs-to-load-path))
         load-path)))

(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)

(load (concat dotfiles-dir "modes.el"))
(load (concat dotfiles-dir "sets.el"))
(load (concat dotfiles-dir "general.el"))
(load (concat dotfiles-dir "key_bindings.el"))
(load (concat dotfiles-dir "experimental.el"))
