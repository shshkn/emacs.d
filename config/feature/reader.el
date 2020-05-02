;; -*- lexical-binding: t -*-

(use-package nov
  :mode ("\\.epub$" . nov-mode)
  :init
  (setq nov-text-width nil
        nov-save-place-file (expand-file-name "nov-places" ds-dir-data-user))
  :config
  (defun ds--setup-nov ()
    (face-remap-add-relative 'variable-pitch
                             :family "DejaVu Serif"
                             :height 1.07)
    (display-line-numbers-mode -1)
    (visual-line-mode 'disable))
  (add-hook 'nov-mode-hook #'visual-fill-column-mode)
  (add-hook 'nov-mode-hook #'ds--setup-nov))

(use-package pdf-tools
  :mode ("\\.pdf$" . pdf-view-mode)
  :commands (pdf-view-mode)
  :init
  (setq pdf-view-display-size 'fit-page
        pdf-view-resize-factor 1.05)
  :config
  (when (not (file-exists-p pdf-info-epdfinfo-program))
    (pdf-tools-install t t t))
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (auto-revert-mode -1)
              (pdf-view-midnight-minor-mode t)))

  (defun update-pdf-tools-theme-hook-fn ()
    (setq-default pdf-view-midnight-minor-mode t
                  pdf-view-midnight-colors
                  (cons (face-attribute 'default :foreground)
                        (face-attribute 'default :background))))

  (add-hook 'pdf-view-mode-hook #'update-pdf-tools-theme-hook-fn))
