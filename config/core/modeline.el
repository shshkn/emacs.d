(defface ds-mode-line-face-fill-column '((t (:inherit error)))
  "Mode line fill column face."
  :group 'ds-mode-line)

(defface ds-mode-line-face-vc-ok '((t (:inherit success)))
  "Mode line vc ok face."
  :group 'ds-mode-line)

(defface ds-mode-line-face-vc-edited '((t (:inherit warning)))
  "Mode line vc edited face."
  :group 'ds-mode-line)

(defface ds-mode-line-face-vc-conflict '((t (:inherit error)))
  "Mode line vc conflict face."
  :group 'ds-mode-line)

(defface ds-mode-line-face-vc-missing '((t (:inherit highlight)))
  "Mode line vc missing face."
  :group 'ds-mode-line)

(defun ds--mode-line-push-right (rhs)
  "Display empty space between lhs and (`RHS') parts of the mode line."
  (propertize " " 'display
              `((space :align-to (- (+ right right-fringe right-margin)
                                    ,(string-width
                                      (format-mode-line rhs)))))))

(defun ds--mode-line-buffer-name ()
  "Display buffer name"
  (propertize "%b" 'face 'mode-line-emphasis))

(defun ds--mode-line-buffer-status ()
  "Display buffer status."
  (let ((status " "))
    (cond (buffer-read-only
           (setq status "R"))
          ((buffer-modified-p)
           (setq status "*")))
    (propertize status 'face 'warning)))

(defun ds--mode-line-buffer-position ()
  "Display the position in the buffer."
  (concat
   "%2l:"
   (if (>= (current-column) fill-column)
       (propertize "%c" 'face 'ds-mode-line-face-fill-column)
     "%2C")))

(defvar ds--mode-line-buffer-lines 0
  "Cache count-lines")
(make-variable-buffer-local 'ds--mode-line-buffer-lines)

(defun ds--mode-line-count-buffer-lines ()
  (unless (buffer-modified-p)
    (setq ds--mode-line-buffer-lines (count-lines (point-min) (point-max))))
  ds--mode-line-buffer-lines)

(defun ds--mode-line-buffer-position-percent ()
  (format "%d"
          (* (/ (line-number-at-pos)
                (float (ds--mode-line-count-buffer-lines)))
             100)))

(defun ds--mode-line-buffer-lines ()
  (format "%d" (ds--mode-line-count-buffer-lines)))

(defun ds--mode-line-buffer-encoding ()
  "Display encoding of the buffer"
  (when (not (memq buffer-file-coding-system '(utf-8-unix
                                               utf-8)))
    (concat " ["
            (symbol-name buffer-file-coding-system)
            "]")))

(defun ds--mode-line-vc-info ()
  "Display vc info."
  (when (stringp vc-mode)
    (let ((branch (replace-regexp-in-string
                   (format "^ %s." (vc-backend buffer-file-name)) " " vc-mode))
          (state (vc-state buffer-file-name)))
      (propertize branch
                  'face (cond ((memq state '(up-to-date ignored))
                               'ds-mode-line-face-vc-ok)
                              ((memq state '(edited added))
                               'ds-mode-line-face-vc-edited)
                              ((memq state '(removed
                                             needs-merge
                                             needs-update
                                             conflict))
                               'ds-mode-line-face-vc-conflict)
                              ((memq state '(missing
                                             unregistered))
                               'ds-mode-line-face-vc-missing))))))

(defun ds--mode-line-major-mode ()
  "Display the major mode."
  (propertize (concat (format-mode-line mode-name)
                      (when (stringp mode-line-process) mode-line-process))
              'face 'mode-line-emphasis))

(defun ds--mode-line-text-scale-amount ()
  "Display text scale amount if so."
  (when (and (featurep 'face-remap)
             (/= text-scale-mode-amount 0))
    (format " %+d " text-scale-mode-amount)))

(defun ds--mode-line-flymake ()
  (when (and (featurep 'flymake)
             flymake-mode)
    (flymake--mode-line-format)))

(defun ds--build-mode-line ()
  "Set up the mode line."
  `((:eval
     (let* ((lhs (list " "
                       (ds--mode-line-buffer-name)
                       (ds--mode-line-buffer-encoding)
                       " "
                       (ds--mode-line-buffer-status)))

            (rhs (list " "
                       (ds--mode-line-vc-info)
                       " "
                       (ds--mode-line-flymake)
                       " "
                       (ds--mode-line-major-mode)
                       " "
                       (ds--mode-line-buffer-position)
                       " "
                       (ds--mode-line-buffer-lines)
                       " "
                       (ds--mode-line-text-scale-amount)
                       " "
                       mode-line-end-spaces
                       ))

            (mid (ds--mode-line-push-right rhs)))
       (list lhs mid rhs)))))

(setq-default mode-line-format (ds--build-mode-line))
