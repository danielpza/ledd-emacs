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

;; https://stackoverflow.com/a/384346
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
	  (message "A buffer named '%s' already exists!" new-name)
	(progn
	  (rename-file filename new-name 1)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil))))))

;; https://stackoverflow.com/questions/8008211/buffer-local-function-in-elisp
(defmacro ledd/def-local-func (func default)
  (let ((variable (intern (format "%s-func" func))))
    (progn
      `(defvar ,variable ,default "This variable contains buffer local function")
      `(make-variable-buffer-local ',variable)
      `(defun ,func (&rest args)
	 "This function run buffer-local function"
	 (interactive)
	 (if (called-interactively-p 'any)
	     (call-interactively ,variable)
	   (apply ,variable args)))
      ;; `(setq ,variable ,default)
      )
    )
  )

(defun ledd/nofunc ()
  (interactive)
  (message "No function specified"))

(ledd/def-local-func ledd/code-action 'ledd/nofunc)
(ledd/def-local-func ledd/code-rename 'ledd/nofunc)
(ledd/def-local-func ledd/code-definition 'ledd/nofunc)
(ledd/def-local-func ledd/code-references 'ledd/nofunc)
(ledd/def-local-func ledd/rename-file 'rename-file-and-buffer)

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
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(setq make-backup-files nil
      indent-tabs-mode nil
      auto-save-default nil
      standard-indent 2
      js-indent-level 2
      ;; inhibit-startup-screen t
      ring-bell-function 'ignore
      ;; initial-scratch-message (concat initial-scratch-message (concat "emacs-init-time: " (emacs-init-time)))
      initial-major-mode 'org-mode
      debug-on-error nil)

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
   evil-symbol-word-search t
   )
  :config
  (setq evil-want-C-u-scroll t)
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
  ;; https://github.com/company-mode/company-mode/issues/530#issuecomment-226566961
  (defun my-company-active-return ()
    (interactive)
    (if (company-explicit-action-p)
	(company-complete)
      (call-interactively
       (or (key-binding (this-command-keys))
	   (key-binding (kbd "RET"))))))
  (define-key company-active-map (kbd "<return>") #'my-company-active-return)
  (define-key company-active-map (kbd "RET") #'my-company-active-return))

(use-package flycheck
  :straight t
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (global-flycheck-mode))

;; code/lsp
;; (use-package lsp-mode
;;   :straight t
;;   :commands lsp
;;   :config
;;   (setq lsp-auto-guess-root t
;; 	gc-cons-threshold 100000000
;; 	read-process-output-max (* 1024 1024)
;; 	lsp-idle-delay 0.200)
;;   :hook
;;   (lsp-mode . lsp-enable-which-key-integration))

;; (use-package company-lsp
;;   :straight t
;;   :after lsp-mode company
;;   :commands company-lsp
;;   :config
;;   (setq company-minimum-prefix-length 1
;; 	company-idle-delay 0.0
;; 	company-lsp-async t))

;; (use-package lsp-ivy
;;   :straight t
;;   :commands (lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol))

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

;; (use-package ivy-rich
;;   :straight t
;;   :after ivy
;;   :config
;;   (ivy-rich-mode 1))
;; (use-package all-the-icons-ivy
;;   :straight t
;;   :config
;;   (setq all-the-icons-ivy-file-commands
;; 	'(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir projectile-find-file))
;;   (all-the-icons-ivy-setup)
;;   )

(use-package which-key
  :straight t
  :config
  (which-key-mode 1))

(use-package projectile
  :straight t
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action #'projectile-dired)
  (projectile-mode 1))

(use-package all-the-icons
  :straight t)

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
   :states '(normal insert visual)
   :keymaps 'override
   "C-t"    #'projectile-toggle-eshell
   "C-w"    #'kill-current-buffer
   "C-S-X"     #'kill-region
   "C-S-C"     #'kill-ring-save
   "C-S-V"     #'yank
   "C-s"   #'save-buffer)
  ;; (general-define-key
  ;;  :keymaps 'override
  ;;  "C-u"    #'evil-scroll-up
  ;;  )


  (leader-define
    :states '(normal visual)
    :keymaps 'override
    "SPC" 'counsel-M-x

    "TAB" #'evil-switch-to-windows-last-buffer
    ";" #'evilnc-comment-or-uncomment-lines

    ;; projectile
    "p" projectile-command-map
    "p t" #'neotree-project-dir

    ;; window
    "w" evil-window-map
    "w d" #'evil-window-delete

    ;; help
    "h" help-map

    ;; code/lsp
    "c f" #'format-all-buffer
    "c c" #'recenter
    "c F" flycheck-command-map
    "c p" #'counsel-yank-pop
    "c a" #'ledd/code-action
    "c r" #'ledd/code-rename
    "c d" #'ledd/code-definition
    "c D" #'ledd/code-references

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
    ;; "f t" #'treemacs
    "f r" #'counsel-recentf
    "f R" #'ledd/rename-file
    "f f" #'counsel-find-file
    "f d" #'delete-current-file
    "f s" #'save-buffer
    "f p" #'find-init-file
    "f P" #'eval-init-file

    ;; search
    "s b" #'counsel-grep-or-swiper
    "s p" #'counsel-rg

    ;; git
    "g g" #'magit-status
    "g d b" #'magit-diff-buffer-file
    "g l b" #'magit-log-buffer-file
    )
  )

;; ui
(use-package neotree
  :straight t
  :config
  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
	  (file-name (buffer-file-name)))
      (neotree-show)
      (if project-dir
	  (if (neo-global--window-exists-p)
	      (progn
		(neotree-dir project-dir)
		(neotree-find file-name)))
	(message "Could not find git project root."))))
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  ;; (setq projectile-switch-project-action 'neotree-projectile-action)
  ;; (setq treemacs-follow-after-init t
  ;; 	treemacs-is-never-other-window t
  ;; 	treemacs-sorting 'alphabetic-case-insensitive-asc)
  ;; (treemacs-follow-mode 1)
  )

;; (use-package treemacs
;;   :straight t
;;   :commands treemacs
;;   :config
;;   (setq treemacs-follow-after-init t
;; 	treemacs-is-never-other-window t
;; 	treemacs-sorting 'alphabetic-case-insensitive-asc)
;;   (treemacs-follow-mode 1))

;; (use-package treemacs-evil
;;   :straight t
;;   :after treemacs evil)

;; (use-package treemacs-projectile
;;   :straight t
;;   :after treemacs projectile)

;; (use-package treemacs-icons-dired
;;   :straight t
;;   :after treemacs dired all-the-icons
;;   :config (treemacs-icons-dired-mode))

;; (use-package treemacs-magit
;;   :straight t
;;   :after treemacs magit)

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
   "C-l" #'centaur-tabs-forward
   "C-S-h" #'centaur-tabs-backward-group
   "C-S-l" #'centaur-tabs-forward-group))

(use-package sublimity
  :straight t
  :config
  (require 'sublimity-scroll)
  (sublimity-mode 1))

(use-package diff-hl
  :straight t
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :init
  (global-diff-hl-mode)
  (diff-hl-margin-mode)
  :general
  (leader-define
    :states '(normal visual)
    :keymaps 'override
    "g [" #'diff-hl-previous-hunk
    "g ]" #'diff-hl-next-hunk
    )
  )

(use-package prodigy
  :straight t
  :general
  (leader-define
    :states '(normal visual)
    :keymaps 'override
    "P" #'prodigy))

;; lang
(use-package json-mode
  :straight t
  ;;   :mode "\\.json\\'"
  )

(use-package web-mode
  :straight t
  ;; :mode "\\.jsx?\\'" "\\.tsx?\\'"
  )

(use-package typescript-mode
  :straight t
  :mode "\\.index.d.ts\\'" "\\.jsx?\\'" "\\.tsx?\\'"
  :config
  (setq typescript-indent-level 2)
  (add-to-list 'interpreter-mode-alist '("node" . typescript-mode))
  ;; :hook (javascript-mode . typescript-mode)
  ;; :hook (typescript-mode . lsp)
  )

(use-package tide
  :straight t
  :after typescript-mode company flycheck
  :config
  (add-hook 'web-mode-hook
	    (lambda ()
	      (when (string-equal "tsx" (file-name-extension buffer-file-name))
		(setup-tide-mode))))
  (add-hook 'web-mode-hook
	    (lambda ()
	      (when (string-equal "jsx" (file-name-extension buffer-file-name))
		(setup-tide-mode))))
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)
  ;; (flycheck-add-mode 'typescript-tslint 'web-mode)
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    ;; (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1)
    (setq-local ledd/code-rename-func 'tide-rename-symbol
		ledd/code-action-func 'tide-fix
		ledd/code-definition-func 'tide-jump-to-definition
		ledd/code-references-func 'tide-references
		ledd/rename-file-func 'tide-rename-file)
    (setq flycheck-check-syntax-automatically '(save mode-enabled)
	  company-tooltip-align-annotations t
	  tide-completion-ignore-case t
	  tide-completion-detailed t
	  tide-completion-enable-autoimport-suggestions t
	  tide-always-show-documentation t)
    )
  :hook ((typescript-mode . tide-hl-identifier-mode)
	 (typescript-mode . setup-tide-mode))
  :general
  (local-leader-define
    :states '(normal visual)
    :keymaps 'typescript-mode-map
    ;; others
    "r" 'tide-restart-server
    ;; fixes
    "f i" 'tide-organize-imports
    )
  )

(use-package eslintd-fix
  :straight t
  :hook ((typescript-mode . eslintd-fix-mode)))

(use-package markdown-mode
  :straight t
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package vmd-mode
  :straight t
  :after markdown-mode)

(use-package yaml-mode
  :straight t)

(defun my/use-eslint-from-node-modules ()
  (let ((root (locate-dominating-file
	       (or (buffer-file-name) default-directory)
	       (lambda (dir)
		 (let ((eslint (expand-file-name "node_modules/eslint/bin/eslint.js" dir)))
		   (and eslint (file-executable-p eslint)))))))
    (when root
      (let ((eslint (expand-file-name "node_modules/eslint/bin/eslint.js" root)))
	(setq-local flycheck-javascript-eslint-executable eslint)))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(use-package git-link
  :straight t
  :general
  (leader-define
    :states '(normal visual)
    :keymaps 'override
    "g l l" 'git-link
    "g l c" 'git-link-commit
    "g l h" 'git-link-homepage
    ))

(when (file-exists-p (concat user-emacs-directory "custom.el"))
  (load-file (concat user-emacs-directory "custom.el")))
