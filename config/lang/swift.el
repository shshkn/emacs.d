;; -*- lexical-binding: t -*-

(defun ds--swift-mode-hook-fn ()
  (setq-local lsp-enable-text-document-color nil)
  (ds--lsp-enable))

(use-package swift-mode
  :hook (swift-mode . ds--swift-mode-hook-fn))

(use-package lsp-sourcekit
  :after lsp-mode
  :config
  ;; TODO: linux / SOURCEKIT_TOOLCHAIN_PATH
  (setq lsp-sourcekit-executable
        "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))
