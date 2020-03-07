(use-package all-the-icons
  :if (or (display-graphic-p) (daemonp))
  :init
  (setq all-the-icons-scale-factor 0.8))

(use-package all-the-icons-dired
  :after (all-the-icons dired)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :after (all-the-icons ibuffer)
  :config
  (all-the-icons-ibuffer-mode t))

(use-package all-the-icons-ivy
  :after (all-the-icons ivy)
  :config
  (add-to-list 'all-the-icons-ivy-file-commands #'counsel-buffer-or-recentf)
  (add-to-list 'all-the-icons-ivy-file-commands #'counsel-ibuffer)
  (add-to-list 'all-the-icons-ivy-file-commands #'counsel-fzf)
  (all-the-icons-ivy-setup)

  (with-eval-after-load 'counsel-projectile
    (add-to-list 'all-the-icons-ivy-file-commands #'counsel-projectile)
    (add-to-list 'all-the-icons-ivy-file-commands #'counsel-projectile-switch-project)
    (add-to-list 'all-the-icons-ivy-file-commands #'counsel-projectile-find-file)
    (add-to-list 'all-the-icons-ivy-file-commands #'counsel-projectile-find-dir)
    (all-the-icons-ivy-setup)))
