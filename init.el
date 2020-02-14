;; https://stackoverflow.com/questions/17483598/maintaining-multiple-emacs-configurations-at-the-same-time
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;; functions

(defun open-scratch-buffer ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

(defun refresh-packages ()
  (interactive)
  (package-refresh-contents)
  (package-install-selected-packages)
  (package-autoremove))

(defun find-init-file ()
  (interactive)
  (find-file user-init-file))

;; defaults

(fset 'yes-or-no-p 'y-or-n-p)
(global-display-line-numbers-mode)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq make-backup-files nil
      auto-save-default nil
      inhibit-startup-screen t
      initial-scratch-message (concat initial-scratch-message (concat "emacs-init-time: " (emacs-init-time)))
      debug-on-error t)

;; package.el

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq package-selected-packages '(;; core
				  use-package
				  evil
				  projectile

				  ;; treemacs
				  treemacs

				  ;; code
				  format-all

				  ;; ivy
				  ivy
				  counsel

				  ;; magit
				  magit
				  evil-magit


				  ;; keybindings
				  general
				  which-key

				  ;; org
				  org
				  htmlize

				  ;; ui
				  doom-themes
				  all-the-icons

				  ;; lang
				  web-mode

				  ;; finances
				  ledger-mode))

;; use-package

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-compute-statistics t)

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-confirm-babel-evaluate nil)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (ledger .t )
     (ruby . t))))

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t
	ivy-height 20
	ivy-wrap t
	ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (ivy-mode 1))

(use-package projectile
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'ivy))

(use-package format-all)

(use-package magit
  :commands magit-status
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package evil-magit
  :after (magit))

(use-package general
  :config

  (defconst my-leader "SPC")
  (general-create-definer leader-define
    :prefix my-leader)

  (leader-define 'normal
    "h" help-map
    "p" projectile-command-map
    "w" evil-window-map)

  (leader-define 'normal
    "SPC" #'counsel-M-x
    ":" #'counsel-M-x)

  (leader-define 'visual
    ";" #'comment-dwim)

  (leader-define 'normal
    :infix "c"
    "f" #'format-all-buffer)

  (leader-define 'normal
    :infix "d"
    "r" #'refresh-packages)

  (leader-define 'normal
    :infix "f"
    "t" #'treemacs
    "p" #'find-init-file
    "r" #'counsel-recentf
    "f" #'find-file
    "s" #'save-buffer)

  (leader-define 'normal
    :infix "g"
    "g" #'magit-status)

  (leader-define 'normal
    :infix "b"
    "k" #'kill-current-buffer
    "e" #'eval-buffer
    "b" #'ivy-switch-buffer
    "p" #'previous-buffer
    "n" #'next-buffer
    "s" #'open-scratch-buffer))

(use-package which-key
  :config
  (which-key-mode 1)
  (push '((nil . "projectile-\\(.+\\)") . (nil . "\\1"))
	which-key-replacement-alist))

(use-package ledger-mode
  :mode "\\.ledger\\'")

(use-package treemacs
  :commands treemacs)

(when (file-exists-p (concat user-emacs-directory "custom.el"))
  (load-file (concat user-emacs-directory "custom.el")))

(use-package doom-themes
  :config
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config))

(use-package web-mode
  :mode ("\\.erb\\'" . web-mode))

(use-package-report)
