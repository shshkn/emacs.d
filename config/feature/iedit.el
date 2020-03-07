(use-package iedit
  :commands iedit-mode
  :bind (("C-;" . iedit-mode)
         :map iedit-lib-keymap
         ("C-s" . iedit-next-occurrence)
         ("C-r" . iedit-prev-occurrence)
         ("M-r" . iedit-replace-occurrences)))
