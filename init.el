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
      ;; inhibit-startup-screen t
      ring-bell-function 'ignore
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
  (global-company-mode)
  ;; (setq company-minimum-prefix-length 1
  ;; 	company-idle-delay 1)
  )

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


(use-package general
  :general

  ;; transient states
  (general-define-key
   :keymap company-active-map
   "C-j"     #'company-select-next
   "C-k"     #'company-select-previous)
  (general-define-key
   :keymap company-search-map
   "C-j"     #'company-select-next-or-abort
   "C-k"     #'company-select-previous-or-abort)

  ;; general keybindings
  (leader-define
    :states '(normal visual)
    :keymaps 'override
    "SPC" 'counsel-M-x

    "TAB" #'evil-switch-to-windows-last-buffer
    ";" #'evilnc-comment-or-uncomment-lines

    ;; window
    "w" evil-window-map
    "w d" #'evil-window-delete

    ;; help
    "h" help-map

    ;; code
    "c f" #'format-all-buffer

    ;; buffer
    "b d" #'kill-current-buffer
    "b e" #'eval-buffer
    "b b" #'ivy-switch-buffer
    "b r" #'revert-buffer-no-confirm
    "b p" #'previous-buffer
    "b n" #'next-buffer
    "b s" #'open-scratch-buffer

    ;; file
    "f t" #'treemacs
    "f p" #'find-init-file
    "f r" #'counsel-recentf
    "f f" #'counsel-find-file
    "f s" #'save-buffer

    ;; search
    "s b" #'counsel-grep-or-swiper
    "s p" #'counsel-rg

    ;; git
    "g g" #'magit-status
    )
  )

;; ui
(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (load-theme 'doom-dark+ t))

;; ;; keybindings
;; (use-package general
;;   :config
;;   (leader-define 'normal
;;     :infix "f"
;;     "" '(:ignore t :which-key "file")
;;     "t" #'treemacs
;;     "p" #'find-init-file
;;     "r" #'counsel-recentf
;;     "f" #'find-file
;;     "s" #'save-buffer)

;;   ;; toggle
;;   (leader-define 'normal
;;     :infix "t"
;;     "" '(:ignore t :which-key "toggle")
;;     "t" #'treemacs
;;     "w" #'whitespace-mode
;;     "u" #'undo-tree-visualize)

;;   (leader-define 'normal
;;     :infix "s"
;;     "" '(:ignore t :which-key "search")
;;     "b" #'counsel-grep-or-swiper
;;     "p" #'counsel-rg)

;;   (leader-define '(normal visual)
;;     :infix "g"
;;     "" '(:ignore t :which-key "magit")
;;     "g" #'magit-status
;;     "db" #'magit-diff-buffer-file
;;     "lb" #'magit-log-buffer-file)

;;   (leader-define 'normal
;;     :infix "o"
;;     "" '(:ignore t :which-key "open")
;;     "a" #'org-agenda
;;     "d" #'dired)

;;   (leader-define '(normal visual)
;;     :infix "e"
;;     "" '(:ignore t :which-key "eval")
;;     "i" #'eval-init-file
;;     "b" #'eval-buffer)

;;   (leader-define 'normal
;;     :infix "b"
;;     "" '(:ignore t :which-key "buffer")
;;     "k" #'kill-current-buffer
;;     "e" #'eval-buffer
;;     "b" #'ivy-switch-buffer
;;     "r" #'revert-buffer-no-confirm
;;     "p" #'previous-buffer
;;     "n" #'next-buffer
;;     "s" #'open-scratch-buffer)

;;   )

;; (use-package projectile
;;   :config
;;   (setq projectile-completion-system 'ivy)
;;   (leader-define 'normal
;;     "p" '(:keymap projectile-command-map :wk "projectile"))
;;   (projectile-mode 1))

;; (use-package which-key
;;   :config
;;   (which-key-mode 1)
;;   (push '((nil . "projectile-\\(.+\\)") . (nil . "\\1"))
;; 	which-key-replacement-alist)
;;   (push '((nil . "flycheck-\\(.+\\)") . (nil . "\\1"))
;; 	which-key-replacement-alist))

;; (use-package ledger-mode
;;   :mode "\\.ledger\\'")

;; (use-package slim-mode
;;   :mode "\\.slim\\'")

;; (use-package yaml-mode
;;   :mode "\\.yml\\'")

;; ;; treemacs
;; (use-package treemacs
;;   :commands treemacs
;;   :config
;;   (treemacs-follow-mode 1))

;; (use-package treemacs-evil
;;   :after treemacs evil)

;; (use-package treemacs-projectile
;;   :after treemacs projectile)

;; (use-package treemacs-icons-dired
;;   :after treemacs dired
;;   :config (treemacs-icons-dired-mode))

;; (use-package treemacs-magit
;;   :after treemacs magit)

;; ;; ui
;; (use-package doom-themes
;;   :config
;;   (setq doom-themes-treemacs-theme "doom-colors")
;;   (doom-themes-treemacs-config))

;; ;; lang
;; (use-package web-mode
;;   :mode ("\\.erb\\'" . web-mode))

;; (use-package flycheck
;;   :init
;;   (global-flycheck-mode)

;;   :config
;;   (leader-define 'normal
;;     "e n" 'flycheck-next-error
;;     "e p" 'flycheck-previous-error
;;     "c F" flycheck-command-map)
;;   )

;; (use-package company-lsp
;;   :commands company-lsp)

;; (use-package lsp-ivy
;;   :commands lsp-ivy-workspace-symbol)

;; (use-package typescript-mode
;;   :mode ("\\.jsx?\\'" "\\.tsx?\\'"))

;; (use-package lsp-mode
;;   :commands lsp
;;   :hook ((ruby-mode . lsp)
;; 	 (typescript-mode . lsp)
;; 	 (lsp-mode . lsp-enable-which-key-integration))
;;   :general
;;   (leader-define 'normal
;;     "l" '(:keymap lsp-command-map :wk "lsp")))

;; (setq custom-file (concat user-emacs-directory "custom.el"))

;; ;; remove when not calling emacs with -q
;; (when (file-exists-p custom-file)
;;   (load-file custom-file))
