;; NOTE: error: symbol's value is void flyspell-delayed-commands
(setq-default flyspell-delayed-commands nil)

(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :if (setq ispell-program-name (executable-find "aspell"))
  :diminish flyspell-mode
  :bind (:map flyspell-mode-map
              ("C-." . nil)
              ("C-;" . nil))
  :init
  (setq ispell-silently-savep t
        ispell-local-dictionary "en_US"))

(use-package flyspell-correct-ivy
  :after flyspell
  :commands (flyspell-correct-word-generic
             flyspell-correct-previous-word-generic)
  :bind (:map flyspell-mode-map
              ("C-," . flyspell-correct-at-point)))
