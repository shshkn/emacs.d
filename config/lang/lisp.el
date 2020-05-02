;; -*- lexical-binding: t -*-

(add-hook 'lisp-mode-hook 'flymake-mode)
(add-hook 'scheme-mode-hook 'flymake-mode)

(use-package sly
  :defer t
  :init
  (setq inferior-lisp-program "sbcl")
  :config
  (add-to-list 'shackle-rules '("\\*sly-mrepl.*?\\*" :regexp t :select nil :align below)))

(defun ds/insert-elisp-header ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert ";; -*- lexical-binding: t -*-\n\n")))
