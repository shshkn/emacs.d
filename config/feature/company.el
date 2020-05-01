(use-package company
  :defer 1
  :diminish company-mode
  :bind (("M-/" . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ([tab] . company-complete-selection)
         ([return] . nil)
         ("RET" . nil))
  :init
  (setq company-tooltip-limit 10
        company-tooltip-align-annotations t
        company-idle-delay 0.03
        company-minimum-prefix-length 2
        company-show-numbers t
        company-selection-wrap-around t
        company-require-match 'never
        company-auto-complete nil
        company-transformers '(company-sort-by-occurrence)
        company-frontends '(company-pseudo-tooltip-frontend
                            company-preview-if-just-one-frontend
                            company-echo-metadata-frontend)
        company-backends '(company-files (:separate company-yasnippet
                                                    company-capf)))
  :config
  (global-company-mode +1))

(use-package company-prescient
  :after company
  :hook (company-mode . company-prescient-mode))
