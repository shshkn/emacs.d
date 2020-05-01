(defun ds--lsp-enable ()
  (when (featurep 'company)
    (company-mode +1))
  (when (featurep 'yasnippet)
    (yas-minor-mode +1))
  (lsp)
  (ds--lsp-company-backends))

(defun ds--lsp-company-backends ()
  (dolist (backend '(company-lsp
                     company-capf
                     company-files
                     company-yasnippet))
    (setq-local company-backends (remove backend company-backends)))

  (add-to-list 'company-backends '(company-files company-yasnippet company-lsp)))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-log-io nil
        lsp-auto-configure t
        lsp-auto-require-clients nil
        lsp-prefer-flymake t
        lsp-restart 'interactive
        lsp-session-file (expand-file-name "lsp-session" ds-dir-data-user)
        lsp-auto-guess-root nil
        lsp-keep-workspace-alive nil
        lsp-eldoc-render-all nil
        lsp-signature-auto-activate t
        lsp-signature-render-documentation nil
        lsp-enable-imenu t
        lsp-enable-snippet t)
  :config
  (when (and os-is-mac
             (not (boundp 'lsp-clients-clangd-executable)))
    (setq lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd")))

(use-package lsp-ui
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              :map lsp-ui-peek-mode-map
              ([return] . lsp-ui-peek--goto-xref))
  :init
  (setq lsp-ui-doc-enable nil))

(use-package company-lsp
  :commands company-lsp
  :init
  (setq company-lsp-cache-candidates t ;; TODO: cache, otherwise it breaks erlang lsp (?)
        company-lsp-async t
        company-lsp-enable-snippet t))
