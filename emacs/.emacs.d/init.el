;; Configure MELPA
(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(dolist (repo '(("melpa" . "https://melpa.org/packages/")
                ("org" . "https://orgmode.org/elpa/")))
  (add-to-list 'package-archives repo t))

(package-initialize)

;; Ensure that use-package is installed
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; Follow symlinks into version control
;;  (since our literate configuration lives there)
(setq vc-follow-symlinks t)

;; Jump into our literate config
(org-babel-load-file "~/.emacs.d/configuration.org")
