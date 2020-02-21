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

(defun eval-init-file ()
  (interactive)
  (load-file user-init-file))

;; defaults

(fset 'yes-or-no-p 'y-or-n-p)
(global-display-line-numbers-mode)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(setq make-backup-files nil
      indent-tabs-mode nil
      auto-save-default nil
      standard-indent 2
      js-indent-level 2
      inhibit-startup-screen t
      ring-bell-function 'ignore
      initial-scratch-message (concat initial-scratch-message (concat "emacs-init-time: " (emacs-init-time)))
      debug-on-error t)

;; package.el

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

(setq package-selected-packages '(;; core
				  use-package
				  evil
				  projectile
				  undotree
				  company
				  flycheck

				  ;; treemacs
				  treemacs
				  treemacs-evil

				  ;; code
				  format-all

				  ;; also installs ivy and swiper
				  counsel

				  ;; magit
				  magit
				  evil-magit

				  ;; keybindings
				  general
				  which-key

				  ;; lsp-mode
				  lsp-mode
				  company-lsp
				  lsp-ivy

				  ;; org
				  org-plus-contrib
				  evil-org
				  gnuplot

				  ;; ui
				  doom-themes
				  all-the-icons

				  ;; lang
				  web-mode
				  slim-mode
				  yaml-mode

				  ;; finances
				  ledger-mode))

;; use-package

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-compute-statistics t)

(use-package company
  :init
  (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
	company-idle-delay 0.1))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-confirm-babel-evaluate nil
	org-agenda-files (list org-directory)
	org-agenda-skip-scheduled-if-done t
	org-agenda-skip-deadline-if-done t
	org-agenda-skip-timestamp-if-done t
	org-startup-indented t
	org-todo-keywords '((sequence "TODO(t)" "PROJ(p)" "STRT(s)" "WAIT(w)" "|" "DONE(d)" "KILL(k)")))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (ledger .t)
     (gnuplot .t)
     (ruby . t))))

(use-package evil-org
  :after org
  :config
  (evil-org-mode))

(use-package evil-org-agenda
  :after evil-org
  :config
  (evil-org-agenda-set-keys))

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t
	ivy-height 20
	ivy-wrap t)
  (ivy-mode 1))

(use-package undo-tree
  :config
  (setq undo-tree-visualizer-diff t
	undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode))

(use-package projectile
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'ivy))

(use-package format-all
  :commands format-all-buffer)

(use-package magit
  :commands magit-status
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package evil-magit
  :after (magit))

(use-package general
  :config
  (general-define-key "M-x" #'counsel-M-x)

  (general-unbind 'motion
    "SPC")

  (general-create-definer leader-define
    :prefix "SPC")

  (general-create-definer local-leader-define
    :prefix "SPC m")

  (leader-define 'normal
    "h" help-map
    "p" projectile-command-map)

  (leader-define
    :states '(normal treemacs)
    "w" evil-window-map)

  (leader-define 'motion
    "SPC" #'counsel-M-x
    ":" #'counsel-M-x
    "TAB" #'evil-switch-to-windows-last-buffer
    "/" #'counsel-rg)

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

  ;; toggle
  (leader-define 'normal
    :infix "t"
    "t" #'treemacs
    "w" #'whitespace-mode
    "u" #'undo-tree-visualize)

  (leader-define 'normal
    :infix "s"
    "b" #'counsel-grep-or-swiper
    "p" #'counsel-rg)

  (leader-define '(normal visual)
    :infix "g"
    "g" #'magit-status
    "db" #'magit-diff-buffer-file
    "lb" #'magit-log-buffer-file)

  (leader-define 'normal
    :infix "o"
    "a" #'org-agenda
    "d" #'dired)

  (leader-define '(normal visual)
    :infix "e"
    "i" #'eval-init-file
    "b" #'eval-buffer)

  (leader-define 'normal
    :infix "b"
    "k" #'kill-current-buffer
    "e" #'eval-buffer
    "b" #'ivy-switch-buffer
    "r" #'revert-buffer
    "p" #'previous-buffer
    "n" #'next-buffer
    "s" #'open-scratch-buffer)

  (local-leader-define
   :states 'normal
   :keymaps 'org-mode-map
   "e" #'org-export-dispatch))

(use-package which-key
  :config
  (which-key-mode 1)
  (push '((nil . "projectile-\\(.+\\)") . (nil . "\\1"))
	which-key-replacement-alist))

(use-package ledger-mode
  :mode "\\.ledger\\'")

(use-package slim-mode
  :mode "\\.slim\\'")

(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package treemacs
  :commands treemacs
  :config
  (treemacs-follow-mode -1))

(use-package treemacs-evil
  :after treemacs)

(use-package doom-themes
  :config
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config))

(use-package web-mode
  :mode ("\\.erb\\'" . web-mode))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  :init (setq lsp-keymap-prefix "s-l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (ruby-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package company-lsp
  :commands company-lsp)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(setq custom-file (concat user-emacs-directory "custom.el"))

;; remove when not calling emacs with -q
(when (file-exists-p custom-file)
  (load-file custom-file))
