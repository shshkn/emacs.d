;; -*- lexical-binding: t -*-

(use-package smartparens-config
  :ensure smartparens
  :defer 1
  :bind (:map smartparens-mode-map
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)

              ("C-M-n" . sp-next-sexp)
              ("C-M-p" . sp-previous-sexp)

              ("C-M-d" . sp-down-sexp)

              ("C-M-e" . sp-backward-up-sexp)
              ("C-M-a" . sp-backward-down-sexp)

              ("C-M-t" . sp-transpose-sexp)

              ("C-k" . sp-kill-hybrid-sexp)
              ("M-k" . sp-backward-kill-sexp)
              ("C-M-w" . sp-copy-sexp)

              ("M-<delete>" . sp-unwrap-sexp)
              ("M-<backspace>" . sp-backward-unwrap-sexp)

              ("C-<right>" . sp-forward-slurp-sexp)
              ("C-<left>" . sp-forward-barf-sexp)
              ("C-M-<left>" . sp-backward-slurp-sexp)
              ("C-M-<right>" . sp-backward-barf-sexp)

              ("M-D" . sp-splice-sexp)
              ("C-M-<delete>" . sp-splice-sexp-killing-forward)
              ("C-M-<backspace>" . sp-splice-sexp-killing-backward)

              ("C-]" . sp-select-next-thing)
              ("C-<left_bracket>" . sp-select-previous-thing)
              ("C-M-]" . sp-select-next-thing-exchange)
              ("C-M-<left_bracket>" . sp-select-previous-thing-exchange))
  :init
  (setq sp-show-pair-delay 0
        sp-show-pair-from-inside t
        sp-max-pair-length 5
        sp-cancel-autoskip-on-backward-movement nil
        sp-highlight-pair-overlay nil)
  :config
  (electric-pair-mode -1)
  (show-paren-mode -1)
  (smartparens-global-mode +1)
  (show-smartparens-global-mode +1))
