;; -*- lexical-binding: t -*-

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
  :bind (("C-x o" . ds--aw-show-dispatch-help)
         ("M-o" . ace-swap-window))
  :commands (ace-window aw-show-dispatch-help)
  :init
  (setq aw-background t
        aw-scope 'frame
        aw-ignore-on t
        aw-ignored-buffers '("*Calc Trail*" " *LV*")
        aw-keys '(?a ?s ?d ?f ?g)
        aw-dispatch-alist '((?0 aw-delete-window "delete window")
                            (?1 delete-other-windows "delete other")
                            (?2 aw-split-window-vert "split —")
                            (?3 aw-split-window-horz "split |")
                            (?4 aw-split-window-fair "split =")
                            (?x aw-swap-window "swap")
                            (?m aw-move-window "move")
                            (?c aw-copy-window "copy")
                            (?b aw-switch-buffer-other-window "switch buffer")))
  :config
  (set-face-attribute 'aw-leading-char-face nil :foreground "#FF5F00" :weight 'bold :height 1.0)

  (defun ds--aw-show-dispatch-help ()
    (interactive)
    (if (>= (length (aw-window-list)) 3)
        (aw-show-dispatch-help)
      (ace-select-window))))

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

(defun ds-frame-move-to-center (&optional frame)
  (let* ((mon-x (car (frame-monitor-attribute 'geometry frame)))
         (mon-y (cadr (frame-monitor-attribute 'geometry frame)))
         (mon-width (caddr (frame-monitor-attribute 'geometry frame)))
         (mon-height (cadddr (frame-monitor-attribute 'geometry frame)))
         (frame-x (+ (/ (- mon-width (frame-pixel-width)) 2) mon-x))
         (frame-y (+ (/ (- mon-height (frame-pixel-height)) 2) mon-y)))
    (set-frame-position frame frame-x frame-y)))

(defun ds--frame-move-and-resize-default (&optional frame)
  (when (display-graphic-p frame)
    (set-frame-size frame 135 35 nil)
    (run-with-timer 0 nil #'ds-frame-move-to-center frame)))

(defun ds--frame-daemon-move-and-resize-default-hook-fn (&optional frame)
  (when (daemonp)
    (ds--frame-move-and-resize-default frame)))

(add-hook 'desktop-no-desktop-file-hook #'ds--frame-move-and-resize-default)
(add-hook 'after-make-frame-functions #'ds--frame-daemon-move-and-resize-default-hook-fn)

(global-set-key (kbd "C-c w w") #'delete-window)
(global-set-key (kbd "C-c w q") #'ds/delete-window-bottom)
