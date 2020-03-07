(use-package flymake-shellcheck
  :hook ((sh-mode .  flymake-mode)
         (sh-mode .  flymake-shellcheck-load)))
