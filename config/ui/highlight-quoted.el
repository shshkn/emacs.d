;; -*- lexical-binding: t -*-

(use-package highlight-quoted
  :hook ((emacs-lisp-mode
          lisp-mode
          clojure-mode
          racket-mode) . highlight-quoted-mode)
  :config
  (set-face-attribute 'highlight-quoted-quote nil :weight 'normal)
  (set-face-attribute 'highlight-quoted-symbol nil :weight 'normal))
