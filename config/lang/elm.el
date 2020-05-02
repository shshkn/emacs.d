;; -*- lexical-binding: t -*-

(use-package elm-mode
  :hook ((elm-mode . ds--lsp-enable)
         (elm-mode . elm-format-on-save-mode)))
