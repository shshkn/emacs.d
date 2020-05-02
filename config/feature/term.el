;; -*- lexical-binding: t -*-

(use-package eshell-toggle
  :bind (("C-c t" . eshell-toggle)
         :map term-mode-map
         ("C-x k" . kill-buffer-and-window))
  :init
  (setq eshell-toggle-size-fraction 3
        eshell-toggle-use-projectile-root t
        eshell-toggle-run-command nil
        eshell-toggle-init-function #'eshell-toggle-init-ansi-term))

(defun ds--term-mode-hook-fn ()
  (display-line-numbers-mode -1)
  (whitespace-mode -1))

(defun ds--fix-ansi-term-invalid-face-hook-fn (&optional theme color)
  (setq ansi-term-color-vector
        [term term-color-black term-color-red
              term-color-green term-color-yellow term-color-blue
              term-color-magenta term-color-cyan term-color-white]))

(add-hook 'term-mode-hook #'ds--term-mode-hook-fn)
(add-hook 'term-mode-hook #'ds--fix-ansi-term-invalid-face-hook-fn)
