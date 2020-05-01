(defvar config-files
  '(
    "core/defaults.el"
    "core/font.el"
    "core/theme.el"
    "core/modeline.el"
    "core/package.el"
    "core/os.el"
    "core/ivy.el"
    "core/projectile.el"
    "core/undo.el"
    "core/windows.el"
    "core/functions.el"

    "feature/dired.el"
    "feature/imenu.el"
    "feature/flyspell.el"
    "feature/flymake.el"
    "feature/smartparens.el"
    "feature/version-control.el"
    "feature/editorconfig.el"
    "feature/multiple-cursors.el"
    "feature/iedit.el"
    "feature/yasnippet.el"
    "feature/avy.el"
    "feature/expand-region.el"
    "feature/helpful.el"
    "feature/dash-zeal.el"
    "feature/dumb-jump.el"
    "feature/company.el"
    "feature/lsp.el"
    "feature/term.el"
    "feature/reader.el"

    "ui/all-the-icons.el"
    "ui/highlight-numbers.el"
    "ui/highlight-quoted.el"
    "ui/highlight-parentheses.el"
    "ui/rainbow.el"
    "ui/hl-todo.el"

    "lang/org.el"
    "lang/markdown.el"
    "lang/data.el"
    "lang/shell.el"
    "lang/lisp.el"
    "lang/racket.el"
    "lang/c.el"
    "lang/rust.el"
    "lang/erlang.el"
    "lang/elixir.el"
    "lang/elm.el"
    "lang/python.el"
    "lang/javascript.el"
    "lang/web.el"
    "lang/java.el"
    "lang/kotlin.el"
    "lang/docker.el"
    ))

(defvar ds-dir-config-root
  (expand-file-name "config/" user-emacs-directory)
  "Config directory.")

(defvar ds-dir-config-custom-root
  (expand-file-name "config-custom/" user-emacs-directory)
  "Custom config directory.")

(defvar ds-file-config-custom-init (expand-file-name
                                    "init-custom.el"
                                    ds-dir-config-custom-root))

(let ((file-name-handler-alist nil))
  (mapc #'load-file
        (mapcar (lambda (file-name)
                  (expand-file-name file-name ds-dir-config-root))
                config-files))
  (when (file-exists-p ds-file-config-custom-init)
    (load-file ds-file-config-custom-init)))
