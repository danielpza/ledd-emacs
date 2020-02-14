;; functions

(defun open-scratch-buffer ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

(defun refresh-packages ()
  (interactive)
  (package-refresh-contents)
  (package-install-selected-packages)
  (package-autoremove))


;; defaults

(fset 'yes-or-no-p 'y-or-n-p)
(display-line-numbers-mode)

(scroll-bar-mode -1)
(tool-bar-mode -1)

;; package.el

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq package-selected-packages '(;; core
				  use-package
				  evil
				  ivy
				  org
				  magit
				  projectile
				  general

				  ;; ui
				  doom-themes

				  ;; finances
				  ledger-mode))

;; use-package

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-compute-statistics t)

(use-package evil
  :config
  (evil-mode 1))

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t
	ivy-height 20)
  (ivy-mode 1))

(use-package projectile
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'ivy))

(use-package magit
  :commands magit-status
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package general
  :config

  (defconst my-leader "SPC")
  (general-create-definer my-leader-def
    :prefix my-leader)

  (defconst my-local-leader "SPC m")
  (general-create-definer my-local-leader-def
    :prefix my-local-leader)

  (my-leader-def 'normal
    "h" help-map
    "p" projectile-command-map
    "w" evil-window-map)

  (my-leader-def 'normal
    ":" #'execute-extended-command)

  (my-leader-def 'visual
    ";" #'comment-dwim)

  (my-leader-def 'normal
    :infix "f"
    "f" #'find-file
    "s" #'save-buffer)

  (my-leader-def 'normal
    :infix "g"
    "g" #'magit-status)

  (my-leader-def 'normal
    :infix "b"
    "b" #'ivy-switch-buffer
    "p" #'previous-buffer
    "n" #'next-buffer
    "s" #'open-scratch-buffer))

(use-package ledger-mode
  :mode "\\.ledger\\'")

(use-package doom-themes
  :config
  (load-theme 'doom-dark+ t))
