(define-key sh-mode-map (kbd "C-c C-c") nil)
(define-key sh-mode-map (kbd "C-c C-f") nil)

(use-package flymake-shellcheck
  :hook ((sh-mode .  flymake-mode)
         (sh-mode .  flymake-shellcheck-load)))
