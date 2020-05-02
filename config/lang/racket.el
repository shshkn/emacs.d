;; -*- lexical-binding: t -*-

(use-package racket-mode
  :mode "\\.rkt$")

(use-package flymake-racket
  :hook ((racket-mode . flymake-mode)
         (racket-mode . flymake-racket-add-hook)
         (scheme-mode . flymake-racket-add-hook)))
