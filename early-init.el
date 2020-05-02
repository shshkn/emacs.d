;; -*- lexical-binding: t -*-

(setq package-enable-at-startup nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(modify-all-frames-parameters '((vertical-scroll-bars . nil)))

(advice-add 'x-apply-session-resources :override 'ignore)
