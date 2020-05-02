;; -*- lexical-binding: t -*-

(use-package undo-fu
  :bind (("C-/" . undo-fu-only-undo)
         ("C-S-/" . undo-fu-only-redo)
         ("C-?" . undo-fu-only-redo))
  :init
  (setq undo-limit (* 128 1024 1024)
        undo-outer-limit (* 128 1024 1024)
        undo-strong-limit (* 256 1024 1024)))
