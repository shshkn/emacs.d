(use-package winner
  :ensure nil
  :demand t
  :bind (("C-c w u" . winner-undo)
         ("C-c w r" . winner-redo))
  :config
  (winner-mode +1))

(use-package windmove
  :ensure nil
  :bind (("C-c w p" . windmove-swap-states-up)
         ("C-c w n" . windmove-swap-states-down)
         ("C-c w f" . windmove-swap-states-right)
         ("C-c w b" . windmove-swap-states-left)))

(use-package ace-window
  :bind (("C-x o" . ace-window)
         ("M-o" . ace-window)
         ("C-M-o" . ace-swap-window))
  :init
  (setq aw-background t
        aw-scope 'frame
        aw-ignore-on t
        aw-ignored-buffers '("*Calc Trail*" " *LV*")))

(use-package shackle
  :init
  (setq shackle-default-size 16
        shackle-rules
        '((fundamental-mode            :select nil :align below)
          (special-mode                :select t   :align below)
          (messages-buffer-mode        :select nil :align below)
          (help-mode                   :select t   :align right :size 0.35 :same-mode t)
          (Buffer-menu-mode            :select t   :align below :size 0.3)
          (compilation-mode            :select nil :align below)
          (grep-mode                   :select nil :align below)
          (inferior-lisp-mode          :select nil :align below)
          (use-package-statistics-mode :select t   :align right :size 0.5)))
  :config
  (shackle-mode +1))

(defun delete-side-window (side)
  (dolist (window (window-at-side-list (selected-frame) side))
    (delete-window window)))

(defun ds/delete-window-bottom ()
  (interactive)
  (delete-side-window 'bottom))

(global-set-key (kbd "C-c w w") #'delete-window)
(global-set-key (kbd "C-c w q") #'ds/delete-window-bottom)
