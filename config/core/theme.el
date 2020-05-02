;; -*- lexical-binding: t -*-

(defvar ds--theme-light 'tango)
(defvar ds--theme-dark 'wombat)
(defvar ds--theme-current-theme ds--theme-dark)
(defvar ds--theme-current-color 'dark)

(defcustom ds-after-theme-enable-hook nil
  "Hook run after enabling the theme.
The hook is called with the selected theme and color as arguments."
  :type 'hook)

(defun ds--theme-ns-frame (color)
  (when (and (eq system-type 'darwin) (display-graphic-p))
    (modify-frame-parameters nil `((ns-appearance . ,color)
                                   (ns-transparent-titlebar . t)))
    (setq default-frame-alist
          (assoc-delete-all 'ns-appearance default-frame-alist))
    (add-to-list 'default-frame-alist `(ns-appearance . ,color))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))))

(defun ds--theme-save-custom-variables (theme color)
  (let ((theme-var (if (eq color 'dark) 'ds--theme-dark 'ds--theme-light)))
    (unless (eq (symbol-value theme-var) theme)
      (customize-save-variable theme-var theme)))

  (unless (eq ds--theme-current-theme theme)
    (customize-save-variable 'ds--theme-current-theme theme))
  (unless (eq ds--theme-current-color color)
    (customize-save-variable 'ds--theme-current-color color)))

(defun ds--theme-enable-theme (theme color)
  (set-frame-parameter nil 'background-mode color)
  (set-terminal-parameter nil 'background-mode color)
  (ds--theme-ns-frame color)

  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t t)
  (enable-theme theme)
  (run-hook-with-args 'ds-after-theme-enable-hook theme color))

(defun set-theme-light (theme)
  "Set a light color THEME."
  (interactive (list
                (intern (completing-read
                         (format "Select a light theme (current %s): "
                                 ds--theme-light)
                         (custom-available-themes)))))
  (ds--theme-enable-theme theme 'light)
  (ds--theme-save-custom-variables theme 'light))

(defun set-theme-dark (theme)
  "Set a dark color THEME."
  (interactive (list
                (intern (completing-read
                         (format "Select a dark theme (current %s): "
                                 ds--theme-dark)
                         (custom-available-themes)))))
  (ds--theme-enable-theme theme 'dark)
  (ds--theme-save-custom-variables theme 'dark))

(defun light ()
  "Switch to the light color theme."
  (interactive)
  (unless (eq ds--theme-current-color 'light)
    (set-theme-light ds--theme-light)))

(defun dark ()
  "Switch to the dark color theme."
  (interactive)
  (unless (eq ds--theme-current-color 'dark)
    (set-theme-dark ds--theme-dark)))

(defun ds--load-last-used-theme ()
  (ds--theme-enable-theme ds--theme-current-theme ds--theme-current-color))

(if desktop-save-mode
    (progn
      (add-hook 'desktop-after-read-hook #'ds--load-last-used-theme)
      (add-hook 'desktop-no-desktop-file-hook #'ds--load-last-used-theme))
  (add-hook 'after-init-hook #'ds--load-last-used-theme))

;; NOTE: load theme one more time with the first created gui frame
;;       to make sure all the hooks have been run
(when (daemonp)
  (defun ds--theme-daemon-hook-fn ()
    (when (display-graphic-p)
      (ds--load-last-used-theme)
      (remove-hook 'server-after-make-frame-hook #'ds--theme-daemon-hook-fn)))
  (add-hook 'server-after-make-frame-hook #'ds--theme-daemon-hook-fn))
