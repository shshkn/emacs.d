(use-package editorconfig
  :diminish editorconfig-mode
  :hook (prog-mode . editorconfig-mode-apply)
  :init
  (setq editorconfig-lisp-use-default-indent t))
