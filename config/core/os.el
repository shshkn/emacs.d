;; -*- lexical-binding: t -*-

(defvar os-is-linux (eq system-type 'gnu/linux))
(defvar os-is-mac (eq system-type 'darwin))
(defvar os-is-win (eq system-type 'windows-nt))

(when os-is-linux
  (add-to-list 'default-frame-alist '(inhibit-double-buffering . t)))

(when os-is-mac
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super)

  (setq ns-use-thin-smoothing nil
        ns-use-proxy-icon nil
        ns-use-mwheel-momentum t
        ns-use-mwheel-acceleration t)

  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

(use-package exec-path-from-shell
  :if (and os-is-mac (or (display-graphic-p)
                         (daemonp)))
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "FZF_DEFAULT_COMMAND"))
    (add-to-list 'exec-path-from-shell-variables var t))
  (exec-path-from-shell-initialize))
