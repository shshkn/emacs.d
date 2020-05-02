;; -*- lexical-binding: t -*-

(use-package rust-mode
  :mode "\\.rs\\'"
  :hook (rust-mode . ds--lsp-enable)
  :init
  (setq rust-format-on-save t))

(use-package cargo
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode)
  :init
  (setq cargo-process--command-flags "--color never"
        cargo-process--enable-rust-backtrace t))
