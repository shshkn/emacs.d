(defun ds--python-init ()
  (setq python-indent-offset 4))

(use-package python
  :hook ((python-mode . ds--lsp-enable)
         (python-mode . ds--python-init))
  :init
  (setq python-shell-interpreter "python3"))
