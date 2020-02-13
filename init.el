;; functions
(defun open-scratch-buffer ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

;; defaults
(fset 'yes-or-no-p 'y-or-n-p)
(display-line-numbers-mode)

(scroll-bar-mode -1)
(tool-bar-mode -1)

;; packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; (package-refresh-contents)

;; core-packages:
;; evil
;; ivy
;; general
;; use-package

(unless (package-installed-p 'use-package) (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

(use-package evil
  :config
  (evil-mode 1))

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t
        ivy-height 20)
  (ivy-mode 1))

;; notes:
;; general-describe-keybindings: show all general keybindings

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
                 ":" #'execute-extended-command)

  (my-leader-def 'visual
                 ";" #'comment-dwim)

  (my-leader-def 'normal
                 :infix "f"
                 "f" #'find-file)

  (my-leader-def 'normal
                 :infix "b"
                 "b" #'ivy-switch-buffer
                 "p" #'previous-buffer
                 "n" #'next-buffer
                 "s" #'open-scratch-buffer))

;; not-core

;; finance
(use-package ledger-mode)

;; ui
(use-package doom-themes
  :defer t
  :init
  (load-theme 'doom-dark+))
