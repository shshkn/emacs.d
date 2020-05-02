;; -*- lexical-binding: t -*-

(use-package dired
  :ensure nil
  :defer t
  :commands (dired dired-jump)
  :init
  (setq dired-dwim-target t
        dired-recursive-copies 'always
        dired-use-ls-dired nil
        dired-listing-switches "-AlhX --group-directories-first")

  (when os-is-mac
    (if (executable-find "gls")
        (setq insert-directory-program "gls")
      (setq dired-listing-switches "-Al"))))

(use-package dired-filter
  :hook ((dired-mode . dired-filter-mode)
         (dired-mode . (lambda () (setq-local dired-filter-stack nil))))
  :init
  (setq dired-filter-revert 'ask))

(use-package dired-subtree
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)))

(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode))

(use-package dired-sidebar
  :bind (("C-c s" . ds/dired-sidebar-toggle-focus)
         :map dired-sidebar-mode-map
         ("q" . kill-this-buffer))
  :init
  (setq dired-sidebar-theme 'icons
        dired-sidebar-open-file-in-most-recently-used-window nil
        dired-sidebar-should-follow-file t
        dired-sidebar-follow-file-idle-delay 1.5
        dired-sidebar-use-term-integration nil
        dired-sidebar-use-magit-integration t
        dired-sidebar-subtree-line-prefix "  "
        dired-sidebar-width 36)
  :config
  (defun ds/dired-sidebar-toggle-focus ()
    (interactive)
    (if (and (dired-sidebar-showing-sidebar-p)
             (not (eq (current-buffer) (dired-sidebar-buffer))))
        (pop-to-buffer (dired-sidebar-buffer))
      (dired-sidebar-toggle-sidebar)))

  (add-to-list 'aw-ignored-buffers 'dired-sidebar-mode)
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode +1))

              (set (make-local-variable 'truncate-partial-width-windows) t)

              (display-line-numbers-mode -1)
              (whitespace-mode -1))))
