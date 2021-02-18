;; -*- lexical-binding: t -*-

(use-package projectile
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (setq projectile-enable-caching nil
        projectile-indexing-method 'alien
        projectile-known-projects-file (expand-file-name "projectile-projects" ds-dir-data-user)
        projectile-cache-file (expand-file-name "projectile-cache" ds-dir-data-user)
        projectile-require-project-root 'prompt
        projectile-globally-ignored-files '("TAGS" ".DS_Store")
        projectile-globally-ignored-file-suffixes '(".bin" ".o" ".elc" ".pyc"))
  :config
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))
  (projectile-mode +1))
