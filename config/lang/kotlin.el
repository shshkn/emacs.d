(use-package kotlin-mode
  :hook (kotlin-mode . ds--lsp-enable)
  :init
  (setq lsp-clients-kotlin-server-executable (expand-file-name
                                              "kotlin_ls/server/bin/kotlin-language-server"
                                              ds-dir-data-root)))
