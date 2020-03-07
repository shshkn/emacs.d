(use-package sh-script
  :ensure nil
  :bind (:map sh-mode-map
              ("C-c C-c" . nil)
              ("C-c C-f" . nil)))

(use-package flymake-shellcheck
  :hook ((sh-mode .  flymake-mode)
         (sh-mode .  flymake-shellcheck-load)))
