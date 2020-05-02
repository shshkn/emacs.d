;; -*- lexical-binding: t -*-

(use-package dumb-jump
  :bind (("C-c j g" . dumb-jump-go)
         ("C-c j p" . dumb-jump-back)
         ("C-c j q" . dumb-jump-quick-look)
         ("C-c j o" . dumb-jump-go-other-window)
         ("C-c j i" . dumb-jump-go-prompt))
  :init
  (setq dumb-jump-selector 'ivy))
