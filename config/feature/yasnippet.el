(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :diminish yas-minor-mode
  :init
  (setq yas-snippet-dirs '()
        yas-verbosity 0)

  (let ((user-snippets-dir (expand-file-name "snippets/" ds-dir-config-custom-root)))
    (when (file-exists-p user-snippets-dir)
      (setq yas-snippet-dirs `(,user-snippets-dir)))))

(use-package yasnippet-snippets
  :after yasnippet)
