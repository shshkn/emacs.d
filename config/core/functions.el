;; -*- lexical-binding: t -*-

(use-package crux
  :bind (("C-c C-c C-d" . crux-delete-file-and-buffer)
         ("C-c C-c C-r" . crux-rename-file-and-buffer)
         ("C-c C-c d" . crux-duplicate-current-line-or-region)
         ("C-c C-c u" . crux-view-url)
         ("M-<return>" . crux-smart-open-line)
         ("S-<return>" . crux-top-join-line))
  :preface
  (defalias 'dtf #'crux-delete-file-and-buffer "Delete this file and buffer")
  (defalias 'rtf #'crux-rename-file-and-buffer "Rename this file and buffer"))

(use-package move-text
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down)))

(defun sudo-open ()
  (interactive)
  (counsel-find-file (concat "/sudo:root@" (system-name) ":/")))

(global-set-key (kbd "C-M-<return>") #'split-line)
(define-key prog-mode-map (kbd "<return>") #'newline-and-indent)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)

(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-c b b") #'previous-buffer)
(global-set-key (kbd "C-c b f") #'next-buffer)

(global-set-key (kbd "C-c u") #'browse-url-at-point)
