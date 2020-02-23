;; https://stackoverflow.com/questions/17483598/maintaining-multiple-emacs-configurations-at-the-same-time
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;; functions

;; https://emacs.stackexchange.com/a/10349
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(defun open-scratch-buffer ()
  "Open scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

(defun find-init-file ()
  "Go to init file (ie this one)."
  (interactive)
  (find-file user-init-file))

(defun eval-init-file ()
  "Eval init file (ie this one)."
  (interactive)
  (load-file user-init-file))

(defun delete-current-file ()
  "Delete current file."
  (interactive)
  (progn
    (when (buffer-file-name)
      (when (file-exists-p (buffer-file-name))
        (progn
          (delete-file (buffer-file-name)))))
    (kill-buffer (current-buffer))))

(defun projectile-toggle-eshell ()
  "Open or hide projectile eshell."
  (interactive)
  (call-interactively (if (eq major-mode 'eshell-mode) 'bury-buffer 'projectile-run-eshell)))

;; helpers

;; from doom
(defconst minibuffer-maps
  `(minibuffer-local-map
    minibuffer-local-ns-map
    minibuffer-local-completion-map
    minibuffer-local-must-match-map
    minibuffer-local-isearch-map
    read-expression-map
    ivy-minibuffer-map
    ivy-switch-buffer-map)
  "A list of all the keymaps used for the minibuffer.")

;; defaults

(fset 'yes-or-no-p 'y-or-n-p)
(global-display-line-numbers-mode)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(setq make-backup-files nil
      indent-tabs-mode nil
      auto-save-default nil
      standard-indent 2
      js-indent-level 2
      inhibit-startup-screen t
      ring-bell-function 'ignore
      initial-scratch-message (concat initial-scratch-message (concat "emacs-init-time: " (emacs-init-time)))
      debug-on-error t)

;;+straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;;-straight

;; use-package

(straight-use-package 'use-package)

;; core
(use-package general
  :straight t
  :config
  (general-create-definer leader-define :prefix "SPC")
  (general-create-definer local-leader-define :prefix "SPC m"))

;; evil
(use-package evil
  :straight t
  :init
  (setq
   evil-want-C-u-scroll t
   evil-want-keybinding nil
   )
  :config
  (evil-mode 1))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-escape
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :straight t
  :after evil)

;; code
(use-package company
  :straight t
  :config
  (global-company-mode))

(use-package flycheck
  :straight t
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (global-flycheck-mode))

;; code/lsp
(use-package lsp-mode
  :straight t
  :commands lsp
  :hook
  (lsp-mode . lsp-enable-which-key-integration))

(use-package company-lsp
  :straight t
  :after lsp-mode company
  :commands company-lsp
  :config
  (setq company-minimum-prefix-length 1
	company-idle-delay 0.0))

(use-package lsp-ivy
  :straight t
  :commands (lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol))

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

(use-package format-all
  :straight t
  :commands format-all-buffer)

;; magit
(use-package magit
  :straight t
  :commands magit-status
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package evil-magit
  :straight t
  :after magit)

;; editor
(use-package counsel
  :straight t)

(use-package ivy
  :straight t
  :config
  (setq ivy-use-virtual-buffers t
	ivy-height 20
	ivy-wrap t)
  (ivy-mode 1))

(use-package which-key
  :straight t
  :config
  (which-key-mode 1))

(use-package projectile
  :straight t
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode 1))

(use-package all-the-icons
  :straight t)

(use-package treemacs
  :straight t
  :commands treemacs
  :config
  (setq treemacs-follow-after-init t
	treemacs-is-never-other-window t
	treemacs-sorting 'alphabetic-case-insensitive-asc)
  (treemacs-follow-mode 1))

(use-package treemacs-evil
  :straight t
  :after treemacs evil)

(use-package treemacs-projectile
  :straight t
  :after treemacs projectile)

(use-package treemacs-icons-dired
  :straight t
  :after treemacs dired all-the-icons
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :straight t
  :after treemacs magit)

;; keybindings
(use-package general
  :general

  ;; transient states
  (general-define-key
   :keymaps minibuffer-maps
   "C-r"    #'evil-paste-from-register
   "C-V"    #'yank
   "C-j"    #'next-line
   "C-k"    #'previous-line)
  (general-define-key
   :keymap company-active-map
   ;; "RET"     nil
   "C-j"     #'company-select-next
   "C-k"     #'company-select-previous)
  (general-define-key
   :keymap company-search-map
   "C-j"     #'company-select-next-or-abort
   "C-k"     #'company-select-previous-or-abort)

  ;; general keybindings
  (general-define-key
   :states 'insert
   "C-SPC"    #'company-complete)
  (general-define-key
   :states '(normal insert)
   :keymaps 'override
   "C-t"    #'projectile-toggle-eshell
   "C-w"    #'kill-current-buffer)

  (leader-define
    :states '(normal visual)
    :keymaps 'override
    "SPC" 'counsel-M-x

    "TAB" #'evil-switch-to-windows-last-buffer
    ";" #'evilnc-comment-or-uncomment-lines

    ;; projectile
    "p" projectile-command-map

    ;; window
    "w" evil-window-map
    "w d" #'evil-window-delete

    ;; help
    "h" help-map

    ;; code
    "c f" #'format-all-buffer
    "c F" flycheck-command-map
    "c p" #'counsel-yank-pop

    ;; buffer
    "b d" #'kill-current-buffer
    "b e" #'eval-buffer
    "b b" #'ivy-switch-buffer
    "b r" #'revert-buffer-no-confirm
    "b p" #'previous-buffer
    "b n" #'next-buffer
    "b s" #'open-scratch-buffer

    ;; error
    "e n" 'flycheck-next-error
    "e p" 'flycheck-previous-error

    ;; file
    "f t" #'treemacs
    "f p" #'find-init-file
    "f r" #'counsel-recentf
    "f f" #'counsel-find-file
    "f d" #'delete-current-file
    "f s" #'save-buffer

    ;; search
    "s b" #'counsel-grep-or-swiper
    "s p" #'counsel-rg

    ;; git
    "g g" #'magit-status
    "g d b" #'magit-diff-buffer-file
    "g l b" #'magit-log-buffer-file

    ;; lsp
    "l" #'lsp-command-map
    )
  )

;; ui
(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (load-theme 'doom-dark+ t))

(use-package doom-modeline
  :straight t
  :config (doom-modeline-mode 1))

(use-package centaur-tabs
  :straight t
  :demand
  :init
  :config
  (setq
   centaur-tabs-style "rounded"
   centaur-tabs-cycle-scope 'tabs
   centaur-tabs-set-icons t
   centaur-tabs-height 32)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-mode t)
  :general
  (general-define-key
   :keymaps 'override
   [C-iso-lefttab] #'centaur-tabs-backward
   [C-tab] #'centaur-tabs-forward
   "C-h" #'centaur-tabs-backward
   "C-l" #'centaur-tabs-forward))

(use-package sublimity
  :straight t
  :config
  (require 'sublimity-scroll)
  (sublimity-mode 1))

;; lang
(use-package typescript-mode
  :mode "\\.tsx?\\'"
  :hook (typescript-mode . lsp))

(add-hook 'js-mode-hook #'lsp)
