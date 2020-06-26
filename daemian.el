(setq load-path
      (append
       (concat dotfiles-dir "elpa")
       load-path))

(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)

(load (concat dotfiles-dir "modes.el"))
(load (concat dotfiles-dir "sets.el"))
(load (concat dotfiles-dir "util.el"))
(load (concat dotfiles-dir "experimental.el"))
(load (concat dotfiles-dir "key_bindings.el"))

