;; -*- lexical-binding: t -*-

(require 'package)

(defvar ds-dir-packages
  (expand-file-name "packages/" ds-dir-data-root)
  "Emacs packages directory.")

(setq-default tls-checktrust t
              gnutls-verify-error t
              package-user-dir ds-dir-packages
              package-enable-at-startup nil
              package-check-signature 'allow-unsigned
              package-gnupghome-dir (expand-file-name "gnupg" ds-dir-data-root)
              package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                                 ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq-default use-package-always-ensure t
              use-package-compute-statistics t)
