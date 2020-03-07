(use-package js2-mode
  :mode "\\.js$"
  :interpreter "node"
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . ds--lsp-enable)
         (js2-mode . (lambda ()
                       (setq-local mode-name "JS")))))

(use-package typescript-mode
  :mode "\\.ts$"
  :hook (typescript-mode . ds--lsp-enable))
