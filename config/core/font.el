;; -*- lexical-binding: t -*-

(defvar ds--font-family "monospace"
  "Font family name.")

(defvar ds--font-size 12.0
  "Default font size.")

(defun ds--font-set-default-frame-alist (font-family font-size)
  (setq default-frame-alist (assoc-delete-all 'font default-frame-alist))
  (add-to-list 'default-frame-alist
               `(font . ,(format "%s%s%s"
                                 font-family
                                 (if (integerp font-size) ":pixelsize=" "-")
                                 font-size))))

(defun ds--font-update-font (font-family font-size)
  (let ((fspec (font-spec
                :family font-family
                :size font-size
                :weight 'normal)))
    (when (find-font fspec)
      (set-frame-font fspec t t)
      (set-face-attribute 'mode-line nil :font fspec)
      (set-face-attribute 'fixed-pitch nil :font fspec)
      (ds--font-set-default-frame-alist font-family font-size)
      (customize-save-variable 'ds--font-family font-family)
      (customize-save-variable 'ds--font-size font-size))))

(defun ds/set-font (font-family font-size)
  "Set font to FONT-FAMILY and FONT-SIZE."
  (interactive (list
                (completing-read (format
                                  "Select a font (current: %s %s): "
                                  ds--font-family
                                  ds--font-size)
                                 (sort (delete-dups (font-family-list)) #'string<))
                (read-number
                 (format "Enter a font size (current %s): " ds--font-size)
                 ds--font-size)))
  (when (ds--font-update-font font-family font-size)
    (message "Font has been changed: %s %s" font-family font-size)))

(defun ds--font-init-hook-fn ()
  (ds--font-update-font ds--font-family ds--font-size))

(add-hook 'after-init-hook #'ds--font-init-hook-fn)
(add-hook 'server-after-make-frame-hook #'ds--font-init-hook-fn)

(ds--font-set-default-frame-alist ds--font-family ds--font-size)
