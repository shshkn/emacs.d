(add-hook 'lisp-mode-hook 'flymake-mode)
(add-hook 'scheme-mode-hook 'flymake-mode)

(use-package sly
  :defer t
  :init
  (setq inferior-lisp-program "sbcl")
  :config
  (add-to-list 'shackle-rules '("\\*sly-mrepl.*?\\*" :regexp t :select nil :align below)))
