;; -*- lexical-binding: t -*-

(use-package base16-theme
  :no-require t)

(use-package grayscale-theme
  :no-require t
  :init
  (defun update-grayscale-theme-faces-hook-fn (theme color)
    (when (eq theme 'grayscale)
      (set-face-attribute 'ivy-current-match nil :background "#ACACAC")
      (set-face-attribute 'ivy-prompt-match nil :background "#ACACAC")))
  (add-hook 'ds-after-theme-enable-hook #'update-grayscale-theme-faces-hook-fn))

(use-package doom-themes
  :no-require t
  :init
  (setq doom-themes-enable-italic nil
        doom-themes-enable-bold nil))

(defun update-cursor-hook-fn (&optional theme color)
  (run-with-idle-timer 0 nil #'set-cursor-color "#FFAA00"))
(add-hook 'ds-after-theme-enable-hook #'update-cursor-hook-fn)
(add-hook 'after-make-frame-functions #'update-cursor-hook-fn)
(add-hook 'server-after-make-frame-hook #'update-cursor-hook-fn)

(defun update-mode-line-hook-fn (theme color)
  (set-face-attribute 'mode-line nil
                      :height 1.0
                      :family ds--font-family
                      :underline nil
                      :overline nil)
  (set-face-attribute 'mode-line-emphasis nil :slant 'normal)
  (set-face-attribute 'line-number-current-line nil :inverse-video nil))
(add-hook 'ds-after-theme-enable-hook #'update-mode-line-hook-fn)

(defun update-all-the-icons-grayscale-hook-fn (theme color)
  (if (or (eq theme 'base16-grayscale-dark)
          (eq theme 'base16-grayscale-light)
          (eq theme 'grayscale))
      (setq all-the-icons-color-icons nil)
    (setq all-the-icons-color-icons t))
  (when (featurep 'all-the-icons)
    (memoize-restore 'all-the-icons-icon-for-file)
    (memoize-restore 'all-the-icons-icon-for-mode)
    (memoize-restore 'all-the-icons-icon-for-url)
    (memoize 'all-the-icons-icon-for-file)
    (memoize 'all-the-icons-icon-for-mode)
    (memoize 'all-the-icons-icon-for-url)
    (when (and (featurep 'dired-sidebar) (dired-sidebar-showing-sidebar-p))
      (dired-sidebar-redisplay-icons))
    (when (get-buffer "*Ibuffer*")
      (with-current-buffer "*Ibuffer*"
        (ibuffer-update nil)))))
(add-hook 'ds-after-theme-enable-hook #'update-all-the-icons-grayscale-hook-fn)

(defun update-lsp-ui-sideline-face-hook-fn (theme color)
  (when (and (eq color 'light)
             (facep 'lsp-ui-sideline-code-action)
             (string= (face-foreground 'lsp-ui-sideline-code-action) "yellow"))
    (set-face-attribute 'lsp-ui-sideline-code-action nil :foreground (face-foreground 'font-lock-string-face))))
(add-hook 'lsp-mode-hook (apply-partially #'update-lsp-ui-sideline-face-hook-fn nil 'light))
(add-hook 'ds-after-theme-enable-hook #'update-lsp-ui-sideline-face-hook-fn)

(defun update-org-bullets-face-hook-fn (theme color)
  (when (facep 'org-level-8)
    (set-face-attribute 'org-level-1 nil :height 1.0 :width 'condensed :inherit 'unspecified)
    (set-face-attribute 'org-level-2 nil :height 1.0 :width 'condensed :inherit 'unspecified)
    (set-face-attribute 'org-level-3 nil :height 1.0 :width 'condensed :inherit 'unspecified)
    (set-face-attribute 'org-level-4 nil :height 1.0 :width 'condensed :inherit 'unspecified)
    (set-face-attribute 'org-level-5 nil :height 1.0 :width 'condensed :inherit 'unspecified)
    (set-face-attribute 'org-level-6 nil :height 1.0 :width 'condensed :inherit 'unspecified)
    (set-face-attribute 'org-level-7 nil :height 1.0 :width 'condensed :inherit 'unspecified)
    (set-face-attribute 'org-level-8 nil :height 1.0 :width 'condensed :inherit 'unspecified)))
(add-hook 'ds-after-theme-enable-hook #'update-org-bullets-face-hook-fn)

(defun update-left-border-face-hook-fn (theme color)
  (set-face-attribute 'vertical-border nil
                      :foreground (face-background 'mode-line)
                      :background nil)

  (set-face-attribute 'fringe nil :background (face-background 'default))
  (set-face-attribute 'line-number nil :background (face-background 'default)))
(add-hook 'ds-after-theme-enable-hook #'update-left-border-face-hook-fn)

(defun update-highlight-face-hook-fn (theme color)
  (if (eq color 'light)
      (set-face-attribute 'highlight nil :background "#eecccc":foreground "#aa2222")
    (set-face-attribute 'highlight nil :background "#553333":foreground "#ffdddd")))
(add-hook 'ds-after-theme-enable-hook #'update-highlight-face-hook-fn)

(defun update-ansi-term-fix-face-hook-fn (theme color)
  (when (eq major-mode 'term-mode)
    (ds--fix-ansi-term-invalid-face-hook-fn)))
(add-hook 'ds-after-theme-enable-hook #'update-ansi-term-fix-face-hook-fn)

;; Use smaller font size in helper buffers
(defface ds-custom-face-helper-buffer nil
  "Custom face for helper buffers")

(defun ds--custom-set-helper-buffer-face-hook-fn ()
  (face-spec-set
   'ds-custom-face-helper-buffer
   `((t :font ,(font-spec
                :family ds--font-family
                :size (max (if (integerp ds--font-size)
                               12
                             8.0)
                           (funcall (if (integerp ds--font-size)
                                        #'round
                                      #'fround)
                                    (* ds--font-size 0.8)))
                :weight 'normal))))
  (buffer-face-set 'ds-custom-face-helper-buffer))

(dolist (hook '(help-mode-hook
                custom-mode-hook
                compilation-mode-hook
                emacs-lisp-compilation-mode-hook
                helpful-mode-hook
                dired-sidebar-mode-hook
                flymake-diagnostics-buffer-mode-hook
                use-package-statistics-mode-hook))
  (add-hook hook #'ds--custom-set-helper-buffer-face-hook-fn))

;; Watch for macOS appearance changes
(defun ds--macos-watcher-start ()
  "Starts watcher. Returns timer."
  (when os-is-mac
    (run-with-timer 0 300 #'ds--macos-theme-check)))

(defun ds--macos-theme-check ()
  (async-start (lambda ()
                 (shell-command "defaults read -g AppleInterfaceStyle"))
               (lambda (exit-code)
                 (if (eq 1 exit-code) (light) (dark)))))

(setq ds--macos-theme-watcher-timer (ds--macos-watcher-start))
;; (cancel-timer ds--macos-theme-watcher-timer)
