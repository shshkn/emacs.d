(use-package ivy
  :hook (after-init . ivy-mode)
  :diminish ivy-mode
  :bind (("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         ("C-c C-c v" . ivy-switch-view)
         :map ivy-minibuffer-map
         ([escape] . minibuffer-keyboard-quit))
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t
        ivy-height 15
        ivy-fixed-height-minibuffer t
        ivy-do-completion-in-region nil
        ivy-count-format "[%d/%d] "
        ivy-format-function #'ivy-format-function-line
        ivy-magic-slash-non-match-action nil
        ivy-extra-directories nil
        ivy-use-selectable-prompt t
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (counsel-grep . ivy--regex-plus)
                                (counsel-rg . ivy--regex-plus)
                                (counsel-ag . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))
        ivy-more-chars-alist '((counsel-grep . 1)
                               (counsel-rg . 1)
                               (t . 1)))
  :config
  (ido-mode -1)
  (when desktop-save-mode
    (setq desktop-globals-to-save
          (append desktop-globals-to-save '(ivy-history ivy-views)))))

(use-package counsel
  :hook (after-init . counsel-mode)
  :diminish counsel-mode
  :bind (("C-c g" . counsel-rg)
         ("C-x f" . counsel-fzf)
         ([remap execute-extended-command] . counsel-M-x)
         ([remap find-file] . counsel-find-file)
         ([remap switch-to-buffer] . ivy-switch-buffer)
         ([remap describe-function] . counsel-describe-function)
         ([remap describe-variable] . counsel-describe-variable)
         ([remap describe-bindings] . counsel-descbinds)
         ([remap describe-face] . counsel-describe-face))
  :init
  (setq-default counsel-rg-base-command
                "rg --hidden --max-columns 175 --no-heading --line-number --color never --glob '!{node_modules,.git}' %s")
  :config
  (ivy-set-display-transformer 'counsel-describe-variable nil)
  (ivy-set-display-transformer 'counsel-describe-function nil)
  (ivy-set-actions 'counsel-find-file '(("d" delete-file "delete"))))

(use-package swiper
  :defer t
  :bind (([remap isearch-forward] . counsel-grep-or-swiper)
         ([remap isearch-backward] . counsel-grep-or-swiper)))

(use-package counsel-projectile
  :after (ivy projectile)
  :config
  (counsel-projectile-mode +1))

(use-package wgrep
  :no-require t)

(use-package ivy-xref
  :after ivy
  :init
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package prescient
  :defer t
  :init
  (setq prescient-save-file (expand-file-name "prescient-save.el" ds-dir-data-user)
        prescient-history-length 150
        prescient-filter-method '(fuzzy))
  :config
  (prescient-persist-mode +1))

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode +1))
