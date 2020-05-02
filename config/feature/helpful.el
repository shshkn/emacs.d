;; -*- lexical-binding: t -*-

(use-package helpful
  :bind (("C-h q" . helpful-kill-buffers)
         ("C-h k" . helpful-key)
         ("C-h f" . helpful-function)
         ("C-h v" . helpful-variable)
         ("C-h h" . helpful-at-point)
         ("C-h C-h" . helpful-at-point)))
