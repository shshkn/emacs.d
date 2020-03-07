(defun ds--c-mode-hook-fn ()
  (when (not (featurep 'ccls))
    (require 'ccls))
  (ds--lsp-enable))

(use-package cc-mode
  :hook ((c-mode c++-mode objc-mode) . ds--lsp-enable)
  :mode ("\\.mm\\'" . objc-mode)
  :bind (:map c-mode-base-map
              ("C-c c" . compile))
  :init
  (setq-default c-basic-offset 4
                c-backspace-function #'delete-backward-char
                c-default-style '((java-mode . "java")
                                  (awk-mode  . "awk")
                                  (other     . "bsd"))))
(use-package ccls
  :no-require t
  :after projectile
  :init
  (setq ccls-initialization-options
        `(:cache
          (:directory ,(expand-file-name "ccls" ds-dir-data-user))))
  (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")
  (add-to-list 'projectile-project-root-files-top-down-recurring ".ccls")
  (add-to-list 'projectile-project-root-files-top-down-recurring ".ccls-root")
  (add-to-list 'projectile-project-root-files-top-down-recurring "compile_commands.json"))
