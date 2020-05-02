;; -*- lexical-binding: t -*-

(use-package org
  :ensure nil
  :defer t
  :commands (org-mode org-agenda org-capture orgtbl-mode)
  :bind (("C-c o a" . org-agenda)
         ("C-c o c" . org-capture)
         :map org-mode-map
         ("C-c M-p" . org-metaup)
         ("C-c M-n" . org-metadown)
         ("C-c M-b" . org-metaleft)
         ("C-c M-f" . org-metaright)
         ("C-c M-B" . org-shiftmetaleft)
         ("C-c M-F" . org-shiftmetaright)
         ("C-'" . nil))
  :init
  (setq org-archive-location "::* Archived"
        org-directory (expand-file-name "~/.org")
        org-startup-indented t
        org-startup-folded nil
        org-adapt-indentation t
        org-src-fontify-natively t)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "CANCELED(c)")
          (sequence "[ ](T)" "[+](N)" "|" "[X](D)" "[\](C)")))

  (setq org-highest-priority ?A
        org-lowest-priority ?C
        org-default-priority ?C
        org-priority-faces '((?A . (:foreground "#FF7755" :weight bold))
                             (?B . (:foreground "#FFAA33"))
                             (?C . (:foreground "#FFDDEE")))))

(use-package org-agenda
  :ensure nil
  :defer t
  :init
  (setq org-agenda-window-setup 'current-window
        org-agenda-restore-windows-after-quit t
        org-agenda-start-with-follow-mode t
        org-agenda-archives-mode nil
        org-agenda-log-mode-items '(closed clock state)
        org-deadline-warning-days 7
        org-agenda-show-log t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t)

  (setq org-agenda-custom-commands
        '(("z" "Todo / Agenda"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-overriding-header "High priority tasks:")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if
                                               'todo '("DONE")))))
            (todo ""
                  ((org-agenda-overriding-header "All tasks:")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if
                                               'todo '("DONE")))))
            (agenda))))))

(use-package org-protocol
  :ensure nil
  :defer t
  :preface
  (when (daemonp)
    (require 'org-protocol))

  (defun ds--org-capture-frame-p ()
    (string=
     "org-capture"
     (frame-parameter (selected-frame) 'name)))

  (defun ds--org-capture-delete-other-windows ()
    (when (ds--org-capture-frame-p)
      (delete-other-windows)))

  (defun ds--org-capture-delete-frame ()
    (when (and (ds--org-capture-frame-p)
               (not (eq this-command 'org-capture-refile)))
      (delete-frame)))

  (defun ds--org-capture-refile-delete-frame ()
    (when (ds--org-capture-frame-p)
      (delete-frame)))

  :hook ((org-capture-mode . ds--org-capture-delete-other-windows)
         (org-capture-after-finalize . ds--org-capture-delete-frame)
         (org-after-refile-insert . ds--org-capture-refile-delete-frame)))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package htmlize
  :after org)
