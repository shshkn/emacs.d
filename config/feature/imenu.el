;; -*- lexical-binding: t -*-

(use-package imenu-anywhere
  :bind (("C-." . ivy-imenu-anywhere)))

(use-package imenu-list
  :bind (("C-'" . imenu-list-smart-toggle)
         ("C-c l" . imenu-list-smart-toggle))
  :init
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize nil))
