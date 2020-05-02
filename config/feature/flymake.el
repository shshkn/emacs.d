;; -*- lexical-binding: t -*-

(use-package flymake
  :commands (flymake-mode flymake-show-diagnostics-buffer)
  :bind (("C-c f" . flymake-show-diagnostics-buffer))
  :diminish flymake-mode
  :config
  (add-to-list 'shackle-rules
               '(flymake-diagnostics-buffer-mode :select nil :align below)))
