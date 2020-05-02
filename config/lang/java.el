;; -*- lexical-binding: t -*-

(use-package cc-mode
  :hook (java-mode . ds--lsp-enable)
  :init
  (setq lsp-java-server-install-dir (expand-file-name
                                     "java_ls"
                                     ds-dir-data-root)
        lsp-java-workspace-dir (expand-file-name
                                "workspace"
                                lsp-java-server-install-dir)))

;; TODO: lsp's too verbose
