;; -*- lexical-binding: t -*-

(use-package diff-hl
  :defer t
  :hook ((prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode-unless-remote)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (setq diff-hl-side 'left)
  :config
  (diff-hl-flydiff-mode +1))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)
         :map magit-mode-map
         ("RET" . magit-diff-visit-file-other-window))
  :init
  (setq magit-diff-options '("-b")
        magit-process-popup-time 10)
  (setq transient-history-file (expand-file-name "transient/history.el" ds-dir-data-user)))

(use-package magit-todos
  :after magit
  :init
  (setq magit-todos-rg-extra-args '("--hidden"))
  :config
  (magit-todos-mode +1))

(use-package gitignore-mode
  :mode "\\.gitignore$")

(use-package gitconfig-mode
  :mode ("\\.gitconfig$'"
         "\\.git/config$'"
         "\\.gitmodules$'"))

(use-package git-timemachine
  :commands git-timemachine)
