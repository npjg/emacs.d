;;; init.el --- switch between named "perspectives" of the editor

;; Copyright (C) 2019-2020 Nathanael Gentry <nathanael.gentrydb8@gmail.com>
;;
;; Licensed under the same terms as Emacs and under the MIT license.

;; Author: Nathanael Gentry <nathanael.gentrydb8@gmail.com>
;; URL: https://github.com/npjg/emacs.d
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5"))
;; Version: 2.7
;; Created: 2019-
;; By: Nathanael Gentry <nathanael.gentrydb8@gmail.com>
;; Keywords: workspace, convenience, frames

;;; Commentary:

;; This file contains all of my Emacs initalization code.

;;; Code:

(toggle-debug-on-error)

(require 'package)
(require 'url)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(dolist (repo '(("melpa" . "https://melpa.org/packages/")
                ("org" . "https://orgmode.org/elpa/")))
  (add-to-list 'package-archives repo t))
(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t
      gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"
      package-check-signature nil)

;; Sensible defaults.

(setq user-full-name "Nathanael Gentry"
      user-mail-address "nathanael.gentrydb8@gmail.com")

(defun npg/require-and-maybe-download (file url &optional feature default-directory)
  (let* ((default-directory (or default-directory user-emacs-directory))
	 (file (expand-file-name file default-directory)))
    (unless  (file-exists-p file)
      (url-copy-file url file))
    (if feature
	(require feature file)
      (load file))))

(npg/require-and-maybe-download "defaults.el" "https://raw.githubusercontent.com/hrs/sensible-defaults.el/master/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode -1)
(tooltip-mode -1)
(blink-cursor-mode -1)
(toggle-frame-maximized)
(add-to-list 'default-frame-alist '(fullscreen . fullboth))
(global-visual-line-mode)

(setq large-file-warning-threshold 30000000
      custom-file (make-temp-file "emacs-custom")
      backup-directory-alist '(("" . "~/.emacs.d/backup"))
      auto-save-default nil
      create-lockfiles nil
      vc-follow-symlinks t
      ring-bell-function 'ignore
      mouse-autoselect-window t
      default-input-method 'russian-computer
      visible-bell nil
      browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox"
      display-time-default-load-average nil
      display-time-24hr-format t
      save-abbrevs 'silently
      column-number-mode t)
(setq-default tab-width 2
              indent-tabs-mode nil)
(push 'split-window-right after-make-frame-functions)
(add-hook 'eww-mode-hook 'hl-line-mode)
(display-time-mode)

(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))
(global-set-key (kbd "C-c w") 'whitespace-mode)

(setq gdb-many-windows t
      gdb-show-main t)

(add-hook 'diff-mode-hook (lambda ()
                            (setq-local whitespace-style
                                        '(face
                                          tabs
                                          tab-mark
                                          spaces
                                          space-mark
                                          trailing
                                          indentation::space
                                          indentation::tab
                                          newline
                                          newline-mark))
                            (whitespace-mode 1)))

(defun npg/set-frame-font (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (set-frame-font "-PfEd-DejaVu Sans Mono-normal-normal-normal-*-9-*-*-*-m-0-iso10646-1")))
(add-hook 'after-make-frame-functions #'npg/set-frame-font)
(npg/set-frame-font)

(defun hrs/opacity (value)
  "Sets the percent opacity of the frame window."
  (interactive "nOpacity Value (0 - 100):")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun npg/advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(defun npg/shell-command-to-string (&rest command)
  "Call a shell command with args in COMMAND separated by spaces,
and remove a trailing newline from the output."
  (replace-regexp-in-string "\n\\'" ""
                            (shell-command-to-string (mapconcat
                                                      'identity command " "))))

(defun npg/split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun npg/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'npg/split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'npg/split-window-right-and-switch)
(global-set-key (kbd "C-h C-u") 'find-function)

(dolist (lines-mode
         '(prog-mode-hook LaTeX-mode-hook LilyPond-mode-hook))
  (add-hook lines-mode #'display-line-numbers-mode))

;; Packages.

(use-package nord-theme
  :load-path "themes"
  :init
  (defun npg/toggle-theme-location ()
    "Easily switch between an inside-friendly and outside-friendly theme."
    (interactive)
    (load-theme (if (memq npg/outside-theme
    custom-enabled-themes) npg/inside-theme npg/outside-theme)))
  (defun npg/load-theme-and-disable-old-theme (theme &rest args)
    "Disable current theme completely before loading a new one."
    (mapcar #'disable-theme custom-enabled-themes))
  (advice-add 'load-theme :before #'npg/load-theme-and-disable-old-theme)
  :config
  (global-set-key (kbd "C-c T") #'npg/toggle-theme-location)
  (setq npg/inside-theme 'nord
        npg/outside-theme 'dichromacy)
  (load-theme 'nord t))

(use-package helm
  :init
  (progn
    (require 'helm-config)
    (require 'helm-grep)
    ;; To fix error at compile:
    ;; Error (bytecomp): Forgot to expand macro with-helm-buffer in
    ;; (with-helm-buffer helm-echo-input-in-header-line)
    (if (version< "26.0.50" emacs-version)
        (eval-when-compile (require 'helm-lib)))

    (defun helm-hide-minibuffer-maybe ()
      (when (with-helm-buffer helm-echo-input-in-header-line)
        (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
          (overlay-put ov 'window (selected-window))
          (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                  `(:background ,bg-color :foreground ,bg-color)))
          (setq-local cursor-type nil))))

    (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
    ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
    ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
    ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))

    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
    (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

    (define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
    (define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
    (define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

    (setq helm-google-suggest-use-curl-p t
          helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
          ;; helm-quick-update t ; do not display invisible candidates
          helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.

          ;; you can customize helm-do-grep to execute ack-grep
          ;; helm-grep-default-command "ack-grep -Hn --smart-case --no-group --no-color %e %p %f"
          ;; helm-grep-default-recurse-command "ack-grep -H --smart-case --no-group --no-color %e %p %f"
          helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window

          helm-echo-input-in-header-line t

          helm-candidate-number-limit 500 ; limit the number of displayed canidates
          helm-ff-skip-boring-files t
          helm-ff-file-name-history-use-recentf t
          helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
          helm-buffer-skip-remote-checking t

          helm-mode-fuzzy-match t

          helm-buffers-fuzzy-matching t ; fuzzy matching buffer names when non-nil
                                        ; useful in helm-mini that lists buffers
          helm-org-headings-fontify t
          ;; helm-find-files-sort-directories t
          ;; ido-use-virtual-buffers t
          helm-semantic-fuzzy-match t
          helm-M-x-fuzzy-match t
          helm-imenu-fuzzy-match t
          helm-lisp-fuzzy-completion t
          ;; helm-apropos-fuzzy-match t
          helm-buffer-skip-remote-checking t
          helm-locate-fuzzy-match t
          helm-display-header-line nil)

    (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)
    (global-set-key (kbd "C-x b") 'helm-buffers-list)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "C-c r") 'helm-recentf)
    (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
    (global-set-key (kbd "C-c h o") 'helm-occur)
    (global-set-key (kbd "C-c h o") 'helm-occur)
    (global-set-key (kbd "C-x f") 'helm-for-files)

    (global-set-key (kbd "C-c h w") 'helm-wikipedia-suggest)
    (global-set-key (kbd "C-c h g") 'helm-google-suggest)

    (global-set-key (kbd "C-c h x") 'helm-register)
    ;; (global-set-key (kbd "C-x r j") 'jump-to-register)

    (define-key 'help-command (kbd "C-f") 'helm-apropos)
    (define-key 'help-command (kbd "r") 'helm-info-emacs)
    (define-key 'help-command (kbd "C-l") 'helm-locate-library)

    ;; use helm to list eshell history
    (add-hook 'eshell-mode-hook
              #'(lambda ()
                  (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

;;; Save current position to mark ring
    (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

    ;; show minibuffer history with Helm
    (define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)
    (define-key minibuffer-local-map (kbd "M-n") 'helm-minibuffer-history)

    (define-key global-map [remap find-tag] 'helm-etags-select)

    (define-key global-map [remap list-buffers] 'helm-buffers-list)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; PACKAGE: helm-swoop                ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Locate the helm-swoop folder to your path
    (use-package helm-swoop
      :bind (("C-c h o" . helm-swoop)
             ("C-c s" . helm-multi-swoop-all))
      :config
      ;; When doing isearch, hand the word over to helm-swoop
      (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

      ;; From helm-swoop to helm-multi-swoop-all
      (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

      ;; Save buffer when helm-multi-swoop-edit complete
      (setq helm-multi-swoop-edit-save t)

      ;; If this value is t, split window inside the current window
      (setq helm-swoop-split-with-multiple-windows t)

      ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
      (setq helm-swoop-split-direction 'split-window-vertically)

      ;; If nil, you can slightly boost invoke speed in exchange for text color
      (setq helm-swoop-speed-or-color t))

    (helm-mode 1)
    (helm-autoresize-mode)

    (use-package helm-projectile
      :init
      (helm-projectile-on)
      (setq projectile-completion-system 'helm)
      (setq projectile-indexing-method 'alien))

    (use-package helm-org-rifle)))

(use-package helm-gtags
  :init
  (progn
    (setq helm-gtags-ignore-case t
          helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t
          helm-gtags-pulse-at-cursor t
          helm-gtags-prefix-key "\C-cg"
          helm-gtags-suggested-key-mapping t)

    ;; Enable helm-gtags-mode in Dired so you can jump to any tag
    ;; when navigate project tree with Dired
    (add-hook 'dired-mode-hook 'helm-gtags-mode)

    ;; Enable helm-gtags-mode in Eshell for the same reason as above
    (add-hook 'eshell-mode-hook 'helm-gtags-mode)

    ;; Enable helm-gtags-mode in languages that GNU Global supports
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'java-mode-hook 'helm-gtags-mode)
    (add-hook 'asm-mode-hook 'helm-gtags-mode)

    ;; key bindings
    (with-eval-after-load 'helm-gtags
      (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
      (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
      (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
      (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
      (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
      (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))))

(use-package which-key
  :init
  (which-key-mode)
  (setq which-key-idle-delay 0.2))

(use-package perspective
  :init
  (npg/require-and-maybe-download "helm-perspective.el"
"https://github.com/dzop/helm-perspective/blob/master/helm-perspective.el" 'helm-perspective)
  (setq persp-show-modestring nil
        persp-initial-frame-name "/")
  (defun persp-add-and-switch-to-buffer (buffer)
    "Associate BUFFER with the current perspective and switch to
it in the current window."
    (interactive
     (list
      (let ((read-buffer-function nil))
        (read-buffer "Add buffer to raise: "))))
    (persp-add-buffer buffer)
    (switch-to-buffer buffer))
  (defun persp-save-some-buffers ()
    "Execute `save-some-buffers' on the current perspective's buffers only."
    (let ((bufs (persp-buffers (persp-curr))))
      (save-some-buffers nil (lambda () (memq (current-buffer) bufs)))))
  (add-hook 'persp-killed-hook #'persp-save-some-buffers)
  (persp-mode)
  :config
  (define-key perspective-map (kbd "a") 'persp-add-and-switch-to-buffer))

(use-package org
  :ensure org-plus-contrib
  :init
  (setq org-tree-root "/home/npgentry/org/index.org")
  (require 'ox-md)
  (require 'ox-beamer)
  (require 'org-expiry)
  (require 'org-global-props "/home/npgentry/org/data/e2/efd81a-556f-48c2-a614-c3b56c8c595e/org-global-props/org-global-props.el")
  (require 'org-title-mode "/home/npgentry/org/data/a8/e760d1-a2e6-445b-adb5-d6eba2825cb1/org-title-mode/org-title-mode.el")
  (require 'org-tree "~/org/data/2f/4624f3-4d4e-4f3c-bd4c-01df3c6dadc0/org-tree/org-tree.el")
  (require 'org-tree-capture "~/org/data/2f/4624f3-4d4e-4f3c-bd4c-01df3c6dadc0/org-tree/org-tree-capture.el")
  (require 'org-tree-magit "~/org/data/2f/4624f3-4d4e-4f3c-bd4c-01df3c6dadc0/org-tree/org-tree-magit.el")
    (require 'org-tree-refile "~/org/data/2f/4624f3-4d4e-4f3c-bd4c-01df3c6dadc0/org-tree/org-tree-refile.el")
 (require 'org-tree-perspective "~/org/data/2f/4624f3-4d4e-4f3c-bd4c-01df3c6dadc0/org-tree/org-tree-perspective.el")
 (org-tree-lookup-table)
 (org-tree-mode)
  (dolist (mode-hook '(org-indent-mode
                       yas-minor-mode
                       npg/yas-activate-latex-extra-mode
                       visual-line-mode
                       org-indent-mode
                       turn-on-auto-fill
                       org-title-mode))
    (add-hook 'org-mode-hook mode-hook))
  (add-hook 'org-capture-mode-hook 'auto-fill-mode)
  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-expiry)
  (advice-add 'org-agenda-redo :after 'org-save-all-org-buffers)
  (advice-add 'org-meta-return :override #'modi/org-meta-return)
  (defun npg/org-end-of-meta-data (arg)
    "An interactive call to `org-end-of-meta-data', with
supported prefix argument."
    (interactive "P")
    (org-end-of-meta-data arg))
  (define-key org-mode-map (kbd "M-n") #'npg/org-end-of-meta-data)
  (defun npg/task-created-insert ()
    (unless current-prefix-arg
      (save-excursion
        (org-back-to-heading)
        (org-expiry-insert-created))))
  (add-hook 'org-insert-heading-hook
            #'npg/task-created-insert)
  :bind (("\C-c a" . org-agenda)
         ("\C-c c" . org-capture))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t)
     (R . t)
     (lilypond . t)
     (python . t)
     (shell . t)
     (dot . t)
     (gnuplot . t)))
  (add-to-list 'org-src-lang-modes '("json" . json))
  (add-to-list 'org-file-apps (cons "\\.pdf\\'" "evince %s"))
  (add-to-list 'org-latex-classes
               '("turabian"
                 "\\documentclass{turabian-researchpaper}"
                 ("\\part{%s}" . "\\part*{%s}")
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  (add-to-list 'org-latex-classes
               '("amsart"
                 "\\documentclass{amsart}"
                 ("\\part{%s}" . "\\part*{%s}")
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  (setq org-indirect-buffer-display 'currrent-window
        org-attach-directory "/home/npgentry/org/data"
        org-archive-location "::* Archive   :ARCHIVE:"
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
        org-src-window-setup 'split-window-below
        org-latex-default-class "amsart"
        org-html-postamble nil
        org-highlight-latex-and-related '(latex script entities)
        org-expiry-created-property-name "CREATED"
        org-expiry-inactive-timestamps t
        org-enforce-todo-depedencies t
        org-agenda-dim-blocked-tasks nil
        org-agenda-sticky t
        org-agenda-restore-windows-after-quit t
        org-agenda-compact-blocks nil
        org-use-fast-todo-selection t
        org-treat-S-cursor-todo-selection-as-state-change nil
        org-log-done 'time
        org-refile-targets '((nil :maxlevel .3)
                             (org-agenda-files :maxlevel . 3))
        org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil
        org-drawers '("PROPERTIES" "LOGBOOK")
        org-clock-into-drawer t
        org-clock-mode-line-total 'current
        org-clock-out-remove-zero-time-clocks t
        org-clock-rounding-minutes 5
        org-clock-in-resume t
        org-clock-persist t
        org-clock-in-resume t
        org-clock-persist-query-resume nil
        org-clock-auto-clock-resolution 'when-no-clock-is-running
        org-clock-out-when-done t
        org-clock-report-include-clocking-task t
        org-columns-default-format
        "%32ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM %16TIMESTAMP_IA"
        org-global-properties '((
         (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
          ("STYLE_ALL" . "habit"))))
        org-attach-auto-tag nil
        org-attach-use-inheritance t
        org-refile-allow-creating-parent-nodes 'confirm
        org-ellipsis " ▼ ")
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "IN(i)")
          (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "DROP(c@/!)" "MEET")))
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "light salmon" :weight bold)
          ("NEXT" :foreground "SteelBlue1" :weight bold)
          ("DONE" :foreground "light green" :weight bold)
          ("IN"   :foreground "olive drab" :weight bold)
          ("WAIT" :foreground "MediumOrchid1" :weight bold)
          ("HOLD" :foreground "Orchid1" :weight bold)
          ("DROP" :foreground "VioletRed1" :weight bold)
          ("MEET" :foreground "light green" :weight bold)))
  (setq org-todo-state-tags-triggers
        (quote (("DROP" ("DROP" . t))
                ("WAIT" ("WAIT" . t))
                ("HOLD" ("WAIT") ("HOLD" . t))
                (done  ("WAIT") ("HOLD"))
                ("TODO" ("WAIT") ("DROP") ("HOLD"))
                ("NEXT" ("WAIT") ("DROP") ("HOLD"))
                ("DONE" ("WAIT") ("DROP") ("HOLD")))))
  (setq org-capture-templates
        `(
          ("v" "Vocabulary")
          ("vr" "Russian vocabulary" subtree (olp "/Vocabulary/Russian/Vocabulary")
           "* %^{Word} :drill:\n %U\n** Definition \n%^{Definition}")
          ("ve" "English vocabulary" entry (olp "/Vocabulary/English/Vocabulary")
           "* %^{Word} :drill:\n %U\n** Definition \n%^{Definition}")

          ("t" "Todo" entry (file+headline org-tree-root "INBOX")
           "* TODO %?\n%U\n" :clock-in t :clock-resume t)
          ("n" "Next" entry (file+headline org-tree-root "INBOX")
           "* NEXT %?\nDEADLINE: %t\n%U\n%a\n" :immediate-finish t)

          ("d" "Diversions")
          ("do" "Out" entry (olp "/Journal/Days")
           "* %? :OUT:\n%U\n" :clock-in t :clock-resume t)
          ("dm" "Meeting" entry  (olp "/Journal/Days")
           "* %? :MEET:\n%U\n" :clock-in t :clock-resume t)

          ("c" "Configs")
          ("ce" "Emacs" plain (file "~/.emacs.d/init.el")  "" :unnarrowed t)

          ("f" "Financial transaction")
          ("ff" "Blank transaction" plain
           (olp+attach file "/Finances" "2f54d8a1-2855-40d7-8baf-6fbe1ee0f9b3.ledger")
           "%(format-time-string \"%Y/%m/%d\") ! %?" :empty-lines 1)

          ("j" "Journal" entry (olp "/Journal/Days")
           "\n\n* %U \n%?\n" :clock-in t :clock-resume t)
          ))
  (setq org-agenda-time-grid
      (quote
       ((daily today remove-match)
        (800 1200 1600 2000)
        "......" "----------------")))
  (setq org-agenda-custom-commands
      '(("s" "Schedule"
         ((agenda "" ((org-agenda-overriding-header "Today's Schedule:")
                      (org-agenda-span 'day)
                      (org-agenda-ndays 1)
                      (org-agenda-start-on-weekday nil)
                      (org-agenda-start-day "+0d")
                      (org-agenda-todo-ignore-deadlines nil)))
          (tags-todo "INBOX" ((org-agenda-overriding-header "Inbox:")
                              (org-tags-match-list-sublevels nil)))
          (tags-todo "-WAIT-HOLD-DROP/!INBOX"
                     ((org-agenda-overriding-header "Next:")))
          (tags-todo "-WAIT-HOLD-DROP/!NEXT"
                     ((org-agenda-overriding-header "Active:")))
          (tags "ENDOFAGENDA"
                ((org-agenda-overriding-header "")
                 (org-tags-match-list-sublevels nil))))
         ((org-agenda-start-with-log-mode t)
          (org-agenda-log-mode-items '(clock))
          (org-agenda-prefix-format
           '((agenda . " %-12:c%?-12t %(gs/org-agenda-add-location-string)% s")
             (timeline . "  % s")
             (todo . " %(npg/org-agenda-prefix-string) ")
             (tags . " %(npg/org-agenda-prefix-string) ")
             (search . "  %i %-12:c")))
          (org-agenda-todo-ignore-deadlines 'near)
          (org-agenda-todo-ignore-scheduled t)))
        ("a" "Agenda"
         ((agenda "") (alltodo))
         ((org-agenda-ndays 10)
          (org-agenda-start-on-weekday nil)
          (org-agenda-start-day "-1d")
          (org-agenda-start-with-log-mode t)
          (org-agenda-log-mode-items '(closed clock state))))))
  (add-hook 'org-agenda-finalize-hook 'gs/remove-agenda-regions))

(use-package org-wc
  :config
  (setq org-wc-default-link-count 'description
        org-wc-ignored-link-types
        (append org-wc-ignored-link-types '(fn cite autocite))))

(use-package org-bullets
  :init (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package org-drill
  :after org
  :config
  (add-to-list 'org-modules 'org-drill)
  (setq org-drill-add-random-noise-to-intervals-p t
        org-drill-hint-separator "||"
        org-drill-left-cloze-delimiter "<["
        org-drill-right-cloze-delimiter "]>"
        org-drill-learn-fraction 0.25
        org-drill--lapse-very-overdue-entries-p t))

(use-package htmlize)

(load "/home/npgentry/org/data/e2/efd81a-556f-48c2-a614-c3b56c8c595e/org-global-props/org-global-props.el")
(require 'org-global-props)

(load "/home/npgentry/org/data/a8/e760d1-a2e6-445b-adb5-d6eba2825cb1/org-title-mode/org-title-mode.el")
(require 'org-title-mode)

(use-package minions
  :config
  (setq minions-mode-line-lighter ""
        minions-mode-line-delimiters '("" . ""))
  (minions-mode 1))

(use-package company
  :ensure company-math
  :bind ("M-/" . company-complete-common)
  :init (global-company-mode 1)
  (delete 'company-semantic company-backends)
  (setq company-idle-delay 0.2)
  :config (add-to-list 'company-backends 'company-math-symbols-unicode))

(use-package company-quickhelp
  :init (company-quickhelp-mode))

(use-package flycheck)

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq undo-tree-visualizer-diff t
        magit-push-always-verify nil
        git-commit-summary-max-length 50)
  (with-eval-after-load 'magit-remote
    (magit-define-popup-action 'magit-push-popup ?P
                               'magit-push-implicitly--desc
                               'magit-push-implicitly ?p t)))

(use-package ag)

(use-package projectile
  :init
  (global-set-key (kbd "C-c v") 'projectile-ag)
  (setq projectile-completion-system 'helm
        projectile-switch-project-action 'projectile-dired
        projectile-enable-caching t
        projectile-require-project-root nil))

(use-package zygospore
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)
         ("RET" .   newline-and-indent)))

(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  :config
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground "red"
                      :inherit 'error
                      :box t))

(use-package elisp-refs)

(use-package smartparens-config
  :ensure smartparens
  :bind (:map smartparens-mode-map
              ("C-M-f" . sp-next-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("C-M-d" . sp-down-sexp)
              ("C-M-a" . sp-backward-down-sexp)
              ("C-M-u" . sp-up-sexp)
              ("C-M-e" . sp-backward-up-sexp)
              ("C-M-n" . sp-forward-sexp)
              ("C-M-p" . sp-previous-sexp)
              ("C-S-d" . sp-beginning-of-sexp)
              ("C-S-a" . sp-end-of-sexp)
              ("C-M-k" . sp-kill-sexp)
              ("C-M-w" . sp-copy-sexp)
              ("M-<delete>" . sp-unwrap-sexp)
              ("M-<backspace>" . sp-backward-unwrap-sexp)
              ("M-D" . sp-splice-sexp)
              ("C-S-<backspace>" . sp-splice-sexp-killing-around)
              ("C-<right>" . sp-forward-slurp-sexp)
              ("C-<left>" . sp-forward-barf-sexp)
              ("C-S-<left>" . sp-backward-slurp-sexp)
              ("C-S-<right>" . sp-backward-barf-sexp))
  :init
  (require 'smartparens-latex)
  (setq sp-cancel-autoskip-on-backward-movement nil
        smartparens-strict-mode t)

  :config (show-smartparens-global-mode t)
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode))

(use-package autoinsert
  :config
  (setq auto-insert-directory "~/.emacs.d/auto-insert/"
        auto-insert-query nil
        auto-insert-alist nil)
  (auto-insert-mode 1)
  (add-hook 'find-file-hook 'auto-insert)
  (define-auto-insert "\\.tex$" ["latex-auto-insert" npg/autoinsert-yas-expand]))

(use-package yasnippet
  :init (yas-global-mode 1)
  (defun npg/yas-activate-latex-extra-mode ()
    (yas-activate-extra-mode 'latex-mode)))

(use-package synosaurus
  :init
  (setq-default synosaurus-backend 'synosaurus-backend-wordnet)
  (add-hook 'after-init-hook #'synosaurus-mode)
  :bind ("s-S" . synosaurus-lookup))

(use-package flyspell
  :config
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (add-hook 'gfm-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'git-commit-mode-hook 'flyspell-mode))

(use-package pdf-tools
  :bind
  ("C-c C-g" . pdf-sync-forward-search)
  :init
  (pdf-tools-install)
  :config
  (add-hook 'pdf-view-mode-hook #'pdf-view-themed-minor-mode)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward-regexp)
  (setq mouse-wheel-follow-mouse t
        pdf-view-resize-factor 1.00))

(use-package multi-term
  :bind (("C-c t" . multi-term))
  :config
  (setq multi-term-program-switches "--login"
        multi-term-switch-after-close nil
        multi-term-dedicated-select-after-open-p t)
  (add-hook 'pdf-view-mode-hook #'pdf-view-fit-page-to-window)
  (add-to-list 'global-auto-revert-ignore-modes 'pdf-view-mode))

(use-package tex-site
  :ensure auctex
  :ensure bibtex
  :ensure cdlatex
  :init
  (defvar npg/LaTeX-no-autofill-environments
    '("equation" "equation*" "align" "align*" "tikzpicture")
    "A list of LaTeX environment names in which `auto-fill-mode' should be inhibited.")
  (defun npg/texcount-words ()
    "Run `texcount' on the current file."
    (interactive)
    (save-buffer)
    (let* ((this-file (buffer-file-name))
           (enc-str (symbol-name buffer-file-coding-system))
           (enc-opt (cond ((string-match "utf-8" enc-str) "-utf8")
                          ((string-match "latin" enc-str) "-latin1")
                          ("-encoding=guess")))
           (word-count (shell-command-to-string
                        (npg/join " " "texcount"  "-0" enc-opt this-file))))
      (message word-count)))
  (defun npg/LaTeX-auto-fill-function ()
    "This function checks whether point is currently inside one of
the LaTeX environments listed in
`npg/LaTeX-no-autofill-environments'. If so, it inhibits automatic
filling of the current paragraph."
    (let ((do-auto-fill t)
          (current-environment "")
          (level 0))
      (while (and do-auto-fill (not (string= current-environment "document")))
        (setq level (1+ level)
              current-environment (LaTeX-current-environment
              level) do-auto-fill (not (member
              current-environment
              npg/LaTeX-no-autofill-environments))))
      (when do-auto-fill
        (do-auto-fill))))
  (defun npg/LaTeX-setup-auto-fill ()
    "This function turns on auto-fill-mode and sets the function
used to fill a paragraph to `npg/LaTeX-auto-fill-function'."
    (auto-fill-mode)
    (setq auto-fill-function 'npg/LaTeX-auto-fill-function))
  :config
  (setq-default TeX-auto-local
                (expand-file-name "auctex-auto" (npg/shell-command-to-string "mktemp" "-d")))
  (setq TeX-auto-save t
        TeX-save-query nil
        TeX-parse-self t
        TeX-error-overview-open-after-TeX-run t
        TeX-source-correlate-method 'synctex
        TeX-correlate-start-server t
        reftex-plug-into-AUCTeX t
        bibtex-dialect 'biblatex
        TeX-view-program-selection '((output-pdf "pdf-tools"))
        TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view"))
        bibtex-dialect 'biblatex)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  (add-hook 'LaTeX-mode-hook
            #'yas-minor-mode)
  (add-hook 'LaTeX-mode-hook
            #'outline-minor-mode)
  (add-hook 'LaTeX-mode-hook
            #'outline-next-heading)
  (add-hook 'LaTeX-mode-hook
            #'turn-on-cdlatex)
  (add-hook 'LaTeX-mode-hook 'npg/LaTeX-setup-auto-fill)
  (add-hook 'LaTeX-mode-hook (lambda () (define-key
  LaTeX-mode-map "\M-=" 'npg/texcount-words))))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package ledger-mode
  :init
  (setq ledger-report-use-native-highlighting nil
        ledger-clear-whole-transactions 1)
  :mode "\\.ledger\\'")

(use-package elpy
  :init
  (elpy-enable)
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(use-package py-autopep8
  :init (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

(use-package indium
  :init
  (add-hook 'js-mode-hook #'indium-interaction-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(progn
  (require 'cc-mode)
  (require 'semantic)

  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (global-semantic-stickyfunc-mode 1)

  (semantic-mode 1)

  (defun alexott/cedet-hook ()
    (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
    (local-set-key "\C-c\C-s" 'semantic-ia-show-summary))

  (add-hook 'c-mode-common-hook 'alexott/cedet-hook)
  (add-hook 'c-mode-hook 'alexott/cedet-hook)
  (add-hook 'c++-mode-hook 'alexott/cedet-hook)

  ;; Enable EDE only in C/C++
  (require 'ede)
  (global-ede-mode))

(use-package json-mode)

(use-package yaml-mode)

(progn
  (require 'nxml-mode)
  (defun bf/format-xml-region (begin end)
    "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
    (interactive "r")
    (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "nXML formatted"))
  (define-key nxml-mode-map (kbd "C-c C-f") #'bf/format-xml-region))

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

(use-package volatile-highlights
  :init
  (volatile-highlights-mode t))

(use-package clean-aindent-mode
  :init
  (add-hook 'prog-mode-hook 'clean-aindent-mode))

(use-package undo-tree
  :init
  (global-undo-tree-mode 1)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t))

(use-package restart-emacs
  :bind ("C-x M-c" . #'restart-emacs))

;; Custom functions.

(defun modi/org-in-any-block-p ()
  "Return non-nil if the point is in any Org block.

The Org block can be *any*: src, example, verse, etc., even any
Org Special block.

This function is heavily adapted from `org-between-regexps-p'."
  (save-match-data
    (let ((pos (point))
          (case-fold-search t)
          (block-begin-re "^[[:blank:]]*#\\+begin_\\(?1:.+?\\)\\(?: .*\\)*$")
          (limit-up (save-excursion (outline-previous-heading)))
          (limit-down (save-excursion (outline-next-heading)))
          beg end)
      (save-excursion
        ;; Point is on a block when on BLOCK-BEGIN-RE or if
        ;; BLOCK-BEGIN-RE can be found before it...
        (and (or (org-in-regexp block-begin-re)
                 (re-search-backward block-begin-re limit-up :noerror))
             (setq beg (match-beginning 0))
             ;; ... and BLOCK-END-RE after it...
             (let ((block-end-re (concat "^[[:blank:]]*#\\+end_"
                                         (match-string-no-properties 1)
                                         "\\( .*\\)*$")))
               (goto-char (match-end 0))
               (re-search-forward block-end-re limit-down :noerror))
             (> (setq end (match-end 0)) pos)
             ;; ... without another BLOCK-BEGIN-RE in-between.
             (goto-char (match-beginning 0))
             (not (re-search-backward block-begin-re (1+ beg) :noerror))
             ;; Return value.
             (cons beg end))))))

(defun modi/org-split-block ()
  "Sensibly split the current Org block at point."
  (interactive)
  (if (modi/org-in-any-block-p)
      (save-match-data
        (save-restriction
          (widen)
          (let ((case-fold-search t)
                (at-bol (bolp))
                block-start
                block-end)
            (save-excursion
              (re-search-backward "^\\(?1:[[:blank:]]*#\\+begin_.+?\\)\\(?:
              .*\\)*$" nil nil 1)
              (setq block-start (match-string-no-properties 0))
              (setq block-end (replace-regexp-in-string
                               "begin_" "end_" ;Replaces "begin_" with "end_",
                                        ;"BEGIN_" with "END_"
                               (match-string-no-properties 1))))
            ;; Go to the end of current line, if not at the BOL
            (unless at-bol
              (end-of-line 1))
            (insert (concat (if at-bol "" "\n")
                            block-end
                            "\n\n"
                            block-start
                            (if at-bol "\n" "")))
            ;; Go to the line before the inserted "#+begin_ .." line
            (beginning-of-line (if at-bol -1 0)))))
    (message "Point is not in an Org block")))

(defun modi/org-meta-return (&optional arg)
  "Insert a new heading or wrap a region in a table.

Calls `org-insert-heading', `org-insert-item',
`org-table-wrap-region', or `modi/org-split-block' depending on
context.  When called with an argument, unconditionally call
`org-insert-heading'."
  (interactive "P")
  (org-check-before-invisible-edit 'insert)
  (or (run-hook-with-args-until-success 'org-metareturn-hook)
      (call-interactively (cond (arg #'org-insert-heading)
                                ((org-at-table-p) #'org-table-wrap-region)
                                ((org-in-item-p) #'org-insert-item)
                                ((modi/org-in-any-block-p) #'modi/org-split-block)
                                (t #'org-insert-heading)))))

(defun npg/display-startup-echo-area-message ()
  (message "Init Time: %s" (emacs-init-time)))

(defun npg/find-function (arg)
  (interactive "P")
  (if arg (call-interactively
           #'find-function-other-window) (call-interactively
           #'find-function)))

(define-key global-map (kbd "C-x M-f") #'npg-find-function)

(defun npg/autoinsert-yas-expand ()
  "Call `yas-expand-snippet' on the whole buffer. Provides
interactivitiy for `autoinsert' templates."
  (yas-expand-snippet (buffer-string) (point-min) (point-max))
  (when (and (boundp 'evil-mode) evil-mode) (call-interactively #'evil-insert)))

(defun hrs/region-or-word ()
  (if mark-active
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    (thing-at-point 'word)))

(defun hrs/dictionary-prompt ()
  (read-string
   (format "Word (%s): " (or (hrs/region-or-word) ""))
   nil
   nil
   (hrs/region-or-word)))

(defun hrs/dictionary-define-word ()
  (interactive)
  (let* ((word (hrs/dictionary-prompt))
         (buffer-name (concat "Definition: " word)))
    (split-window-below)
    (with-output-to-temp-buffer buffer-name
      (shell-command (format "sdcv -n %s" word) buffer-name))))

(define-key global-map (kbd "C-x w-w") #'hrs/dictionary-define-word)

(defun npg/nemo-default-directory ()
  (interactive)
  (async-shell-command (format "nemo %s" default-directory)))

(define-key global-map (kbd "C-x n f") #'npg/nemo-default-directory)

(defun gs/org-agenda-next-section ()
  "Go to the next section in an org agenda buffer"
  (interactive)
  (if (search-forward "===" nil t 1)
      (forward-line 1)
    (goto-char (point-max)))
  (beginning-of-line))

(defun gs/org-agenda-prev-section ()
  "Go to the next section in an org agenda buffer"
  (interactive)
  (forward-line -2)
  (if (search-forward "===" nil t -1)
      (forward-line 1)
    (goto-char (point-min))))

(defun npg/org-agenda-prefix-string ()
  "Format"
  (let ((path (org-format-outline-path (org-get-outline-path))) ; "breadcrumb" path
        (stuck nil)) ;;(gs/org-agenda-project-warning))) ; warning for stuck projects
    (if (> (length path) 0)
        (concat stuck ; add stuck warning
                " [" path "]") ; add "breadcrumb"
      stuck)))

(defun gs/org-agenda-add-location-string ()
  "Gets the value of the LOCATION property"
  (let ((loc (org-entry-get (point) "LOCATION")))
    (if (> (length loc) 0)
        (concat "{" loc "} ")
      "")))

(defun bh/narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree)
  (save-restriction
    (org-agenda-set-restriction-lock)))

(defun bh/narrow-to-subtree ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (org-get-at-bol 'org-hd-marker)
          (bh/narrow-to-org-subtree))
        (when org-agenda-sticky
          (org-agenda-redo)))
    (bh/narrow-to-org-subtree)))

(defun bh/narrow-up-one-org-level ()
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (bh/narrow-to-org-subtree)))

(defun bh/get-pom-from-agenda-restriction-or-point ()
  (or (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
      (org-get-at-bol 'org-hd-marker)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))

(defun bh/narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
          (bh/narrow-up-one-org-level))
        (org-agenda-redo))
    (bh/narrow-up-one-org-level)))

(defun gs/remove-agenda-regions ()
  (save-excursion
    (goto-char (point-min))
    (let ((region-large t))
      (while (and (< (point) (point-max)) region-large)
        (set-mark (point))
        (gs/org-agenda-next-section)
        (if (< (- (region-end) (region-beginning)) 5) (setq region-large nil)
          (if (< (count-lines (region-beginning) (region-end)) 4)
              (delete-region (region-beginning) (region-end))))))))


(find-file org-tree-root)
;; (org-agenda nil " ")
