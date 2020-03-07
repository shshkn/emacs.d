(when os-is-mac
  (use-package dash-at-point
    :bind (("C-c d" . dash-at-point)
           ("C-c e" . dash-at-point-with-docset))))

(when (and os-is-linux
           (executable-find "zeal"))
  (use-package zeal-at-point
    :bind ("C-c d" . zeal-at-point)))
