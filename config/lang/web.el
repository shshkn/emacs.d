;; -*- lexical-binding: t -*-

(use-package sgml-mode
  :hook (html-mode . ds--lsp-enable))

(use-package css-mode
  :hook ((css-mode
          scss-mode) . ds--lsp-enable))

(use-package emmet-mode
  :hook (web-mode css-mode html-mode rjsx-mode)
  :init
  (setq emmet-move-cursor-between-quotes t))
