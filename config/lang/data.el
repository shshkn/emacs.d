;; -*- lexical-binding: t -*-

(use-package json-mode
  :mode ("\\.js\\(on\\|lintrc\\)" "\\.babelrc$")
  :init
  (setq json-reformat:indent-width 2))

(use-package yaml-mode
  :mode "\\.ya?ml$")

(use-package toml-mode
  :mode "\\.toml$")

(use-package protobuf-mode
  :commands (protobuf-mode))
