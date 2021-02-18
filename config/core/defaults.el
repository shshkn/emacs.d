;; -*- lexical-binding: t -*-

(setq garbage-collection-messages t
      gc-cons-threshold most-positive-fixnum)

(defun gc-run ()
  (when (not (minibufferp))
    (message "INFO: GC / gcs-done: %s, gc-elapsed: %s" gcs-done gc-elapsed))
  (garbage-collect))

(run-with-idle-timer 300 t #'gc-run)

(setq large-file-warning-threshold (* 128 1024 1024))

(setq attempt-orderly-shutdown-on-fatal-signal nil
      attempt-stack-overflow-recovery nil)

;; Files

(prefer-coding-system 'utf-8)

(defvar ds-dir-data-root
  (expand-file-name "data/" user-emacs-directory)
  "Emacs data directory.")

(defvar ds-dir-data-user
  (expand-file-name "user/" ds-dir-data-root)
  "User data directory.")

(let ((dirs `(,ds-dir-data-root ,ds-dir-data-user)))
  (mapc (lambda (dir)
          (unless (file-exists-p dir)
            (make-directory dir)))
        dirs))

(setq custom-file (expand-file-name "custom.el" ds-dir-data-user))
(when (file-exists-p custom-file)
  (load-file custom-file))

(setq-local backup-dir (expand-file-name "backup/" ds-dir-data-user))
(setq make-backup-files nil
      backup-directory-alist `((".*" . ,backup-dir)
                               (cons tramp-file-name-regexp nil)))

(setq auto-save-dir (expand-file-name "autosave/" ds-dir-data-user))
(setq auto-save-default nil
      auto-save-file-name-transforms `((".*" ,auto-save-dir t))
      auto-save-list-file-name (expand-file-name "autosave-list" auto-save-dir)
      auto-save-list-file-prefix (expand-file-name ".saves~" auto-save-dir)
      auto-save-interval 5000
      auto-save-timeout 30)

(add-hook 'auto-save-hook (lambda ()
                            (when (and (buffer-modified-p) (buffer-file-name))
                              (save-buffer))))

(setq-default recentf-save-file (expand-file-name "recentf" ds-dir-data-user)
              recentf-list '()
              recentf-exclude '("/tmp" "/ssh:")
              recentf-max-saved-items 200
              recentf-max-menu-items 20)
(recentf-mode +1)

(setq-default save-place-file (expand-file-name "places" ds-dir-data-user)
              save-place-forget-unreadable-files nil)
(save-place-mode +1)

(setq-default savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-file (expand-file-name "history" ds-dir-data-user)
              savehist-autosave-interval 90
              history-length 1500)
(savehist-mode +1)

(setq bookmark-file (expand-file-name "bookmarks" ds-dir-data-user))

(setq-default desktop-path `(,ds-dir-data-user)
              desktop-dirname ds-dir-data-user
              desktop-base-file-name "desktop"
              desktop-base-lock-name "desktop.lock"
              desktop-save t
              desktop-auto-save-timeout 60
              desktop-load-locked-desktop t
              desktop-restore-eager 5
              desktop-restore-frames (not (daemonp)))
(unless (daemonp)
  (desktop-save-mode +1))

(setq ido-everywhere t
      ido-enable-flex-matching t
      ido-save-directory-list-file (expand-file-name "ido.last" ds-dir-data-user))

(setq-default abbrev-file-name (expand-file-name "abbrev_defs" ds-dir-data-user))

(setq-default eshell-directory-name (expand-file-name "eshell" ds-dir-data-user))

(setq create-lockfiles nil)

(setq find-file-visit-truename nil)
(setq vc-follow-symlinks t)

(setq tramp-default-method "sudo"
      tramp-terminal-type "dumb"
      tramp-persistency-file-name (expand-file-name "tramp" ds-dir-data-user))

(setq system-uses-terminfo nil)

;; UI
(setq inhibit-x-resources t)

(setq frame-resize-pixelwise t
      window-resize-pixelwise t
      frame-inhibit-implied-resize t)

(setq-default frame-title-format
              '(:eval
                (cond ((when buffer-file-name
                         (concat
                          (replace-regexp-in-string (expand-file-name "~")
                                                    "~"
                                                    default-directory)
                          "%b")))
                      ("%b"))))

(setq-default idle-update-delay 3)

(tooltip-mode -1)

(setq auto-window-vscroll nil)

(setq-default cursor-type 'box
              cursor-in-non-selected-windows nil)

(blink-cursor-mode -1)

(setq x-stretch-cursor t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq confirm-kill-processes nil)

(setq initial-scratch-message nil
      inhibit-startup-message t)

(defun ds--inhibit-message-hook-fn (&optional frame)
  (setq inhibit-message t)
  (run-with-idle-timer 0.01 nil (lambda ()
                                  (setq inhibit-message nil))))
(add-hook 'server-after-make-frame-hook #'ds--inhibit-message-hook-fn)

(defun display-startup-echo-area-message ()
  (message "Emacs init time: %s" (emacs-init-time)))

(setq use-dialog-box nil
      use-file-dialog nil)

(setq echo-keystrokes 0.15)

(setq ring-bell-function #'ignore)

(setq indicate-empty-lines nil)

(column-number-mode +1)

(setq-default display-line-numbers-type 'absolute
              display-line-numbers-width 3)
(global-display-line-numbers-mode +1)

(fringe-mode '(8 . 0))

(setq-default fill-column 120)

(setq-default show-paren-delay 0)
(show-paren-mode +1)
(electric-pair-mode +1)
(electric-indent-mode +1)

;; Editor
(setq-default indent-tabs-mode nil
              tab-always-indent t
              tab-width 4
              show-trailing-whitespace nil)

(setq-default truncate-lines nil
              truncate-partial-width-windows nil)

(setq-default case-fold-search t)

(setq set-mark-command-repeat-pop t)
(transient-mark-mode +1)
(delete-selection-mode +1)

(setq require-final-newline t)

(setq save-interprogram-paste-before-kill t)

(setq undo-limit (* 128 1024 1024))

(setq-default global-auto-revert-non-file-buffers t
              auto-revert-verbose nil)
(global-auto-revert-mode +1)

(setq-default view-read-only t)

(setq-default enable-recursive-minibuffers nil)

(setq-default compilation-always-kill t
              compilation-ask-about-save nil
              compilation-scroll-output t)

(add-hook 'text-mode-hook #'goto-address-mode)

(global-subword-mode +1)
(with-eval-after-load 'diminish (diminish 'subword-mode))

(setq scroll-step 1
      scroll-margin 1
      scroll-conservatively 101
      scroll-preserve-screen-position t)

(setq mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(5
                                  ((shift) . 1)
                                  ((meta) . nil)
                                  ((control) . text-scale)))

(setq text-scale-mode-step 1.05)
(global-set-key [C-wheel-down] #'text-scale-decrease)
(global-set-key [C-wheel-up] #'text-scale-increase)

;; Disable disabled commands prompt
(setq disabled-command-function #'ignore)

;; Enable some of disabled commands
(dolist (fn '(upcase-region
              downcase-region
              list-threads
              list-timers))
  (put fn 'disabled nil))

;; Allow rebinding of "C-<left_bracket>". By default C-[ is for ESC.
(define-key input-decode-map [?\C-\[] (kbd "C-<left_bracket>"))
(global-set-key [?\C-\M-\[] #'ignore)
(define-key key-translation-map [?\C-\M-\[] (kbd "C-M-<left_bracket>"))

;; Don't open *Messages* buffer by clicking on the minibuffer
(define-key minibuffer-inactive-mode-map [mouse-1] #'ignore)

;; Disable help message when mouse over the mode line
(setq mode-line-default-help-echo #'ignore)
(global-set-key [mode-line mouse-2] #'ignore)
(global-set-key [mode-line mouse-3] #'ignore)

;; Disable context menus
(global-set-key [C-down-mouse-1] #'ignore)
(global-set-key [C-down-mouse-2] #'ignore)
(global-set-key [C-down-mouse-3] #'ignore)

;; Set M-` to cycle through frames
(global-set-key (kbd "M-`") #'other-frame)

;; Unbind suspend-emacs/frame
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x C-z") nil)

(global-set-key (kbd "C-x k") #'kill-this-buffer)
