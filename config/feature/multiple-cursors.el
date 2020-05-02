;; -*- lexical-binding: t -*-

(use-package multiple-cursors
  :bind (("C-c m c" . mc/edit-lines)
         ("C-c m n" . mc/mark-next-like-this)
         ("C-c m p" . mc/mark-previous-like-this)
         ("C-c m a" . mc/mark-all-like-this)
         ("C-c m s" . mc/skip-to-next-like-this)
         ("C-c m S" . mc/skip-to-previous-like-this)
         :map mc/keymap
         ("<return>" . nil))
  :init
  (setq mc/list-file (expand-file-name "mc-lists.el" ds-dir-data-user)))
