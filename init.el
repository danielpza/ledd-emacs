;; functions
(defun open-scratch-buffer ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

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

;; use-package
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; core-packages:
;; evil
;; counsel
;; ivy
;; general
;; use-package
;; which-key

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

(use-package counsel)

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t
        ivy-height 20)
  (ivy-mode 1))

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
                 "w" evil-window-map)

  (my-leader-def 'normal
                 ":" #'counsel-M-x)

  (my-leader-def 'visual
                 ";" #'comment-dwim)

  (my-leader-def 'normal
                 :infix "f"
                 "s" #'save-buffer
                 "f" #'find-file)

  (my-leader-def 'normal
                 :infix "b"
                 "b" #'ivy-switch-buffer
                 "p" #'previous-buffer
                 "n" #'next-buffer
                 "s" #'open-scratch-buffer))

(use-package which-key
  :config
  (which-key-mode 1))

;; not-core

;; finance
(use-package ledger-mode
  :defer t)

;; ui
(use-package doom-themes
  :defer t
  :init
  (load-theme 'doom-dark+ t))
