(use-package avy
  :bind (("M-g f" . avy-goto-line)
         ("C-c a" . avy-goto-whitespace-end))
  :init
  (setq avy-background t)
  :config
  (add-to-list 'avy-ignored-modes 'help-mode)

  (when (featurep 'dired-sidebar)
    (add-to-list 'avy-ignored-modes 'dired-sidebar-mode)))
