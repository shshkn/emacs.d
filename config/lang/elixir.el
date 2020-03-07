(use-package elixir-mode
  :hook (elixir-mode . ds--lsp-enable)
  :init
  (setq lsp-clients-elixir-server-executable (expand-file-name
                                              "elixir_ls/language_server.sh"
                                              ds-dir-data-root)))
