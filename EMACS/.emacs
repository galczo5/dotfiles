(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/")
             t)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/")
             t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq user-mail-address "kamil.galek@gmail.com"
      user-full-name    "Kamil Gałek")

;;Do not show welcome message
(setq vc-follow-symlinks t)
(setq inhibit-startup-message t)
(setq make-backup-files nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

;;Fonts
(set-frame-font "Source Code Pro")
(set-face-attribute 'default nil :height 100)

;;Hide all bars
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1) ;;Sometimes it's usefull
(ido-mode t)

;;Linum mode - line numbers
;; (global-linum-mode 1)
(global-set-key [f7] 'linum-mode)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
;;(setq linum-format " %4d ")

(show-paren-mode 1)
(column-number-mode 1)

(set-default 'cursor-type 'hbar)
(setq fringes-outside-margins t)

;;Uncomment for line highlighting
(global-hl-line-mode 1)
(global-visual-line-mode 1)

(global-set-key (kbd "C-x k") 'kill-buffer-and-window)
(global-set-key (kbd "C-x ;") 'comment-line)

(use-package "doom-themes"
  :ensure t
  :config (load-theme 'doom-one t))

(use-package "column-enforce-mode"
  :ensure t
  :config (column-enforce-mode 1))

(use-package "helm-swoop"
  :ensure t
  :config (global-set-key (kbd "C-s") 'helm-swoop))

(use-package "ws-butler"
  :ensure t
  :config (ws-butler-global-mode))

(use-package "flycheck"
  :ensure t
  :config (global-flycheck-mode))

(use-package "switch-window"
  :ensure t
  :config (global-set-key (kbd "C-x o") 'switch-window))

(use-package "visual-regexp"
  :ensure t
  :config (define-key global-map (kbd "C-c r") 'vr/replace))

(use-package "expand-region"
  :ensure t
  :config (global-set-key (kbd "C-=") 'er/expand-region))

(use-package "smex"
  :ensure t
  :init (smex-initialize)
  :config (global-set-key (kbd "M-x") 'smex))

(use-package "autopair"
  :ensure t
  :config (autopair-global-mode))

(use-package "multiple-cursors"
  :ensure t
  :config (global-set-key (kbd "C-n") 'mc/mark-next-like-this))

(use-package "project-explorer"
  :ensure t
  :config (global-set-key [f8] 'project-explorer-toggle))

(use-package "emmet-mode"
  :ensure t
  :config (global-set-key (kbd "C-e") 'emmet-expand-line))

(use-package "web-mode"
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
  :config (add-hook 'web-mode-hook 'superword-mode)
  )

(use-package "company-web"
  :ensure t
  :config
  (add-hook 'web-mode-hook 'company-web-bootstrap+)
  (add-hook 'web-mode-hook 'company-web-fa+))

(use-package "company"
  :ensure t
  :config
  (global-company-mode 1)
  (global-set-key (kbd "C-SPC") 'company-complete-common)
  (add-to-list 'company-backends 'company-web-html)
  (add-to-list 'company-backends 'company-web-bootstrap+))

(use-package "auto-complete"
  :ensure t)

(use-package "ac-html-bootstrap"
  :ensure t)

(use-package "js2-mode"
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :config
  (add-hook 'js2-mode-hook 'ac-js2-mode)
  (add-hook 'js2-mode-hook 'subword-mode))

(use-package "rainbow-mode"
  :ensure t
  :config (add-hook 'css-mode-hook 'rainbow-mode))

(use-package "yasnippet"
  :ensure t
  :config
  (yas-global-mode t)
  (global-set-key (kbd "C-q") 'company-yasnippet)
  (global-set-key (kbd "C-c C-s") 'yas-insert-snippet))

(use-package "sqlup-mode"
  :ensure t
  :config (add-hook 'sql-mode-hook 'sqlup-mode))

(use-package "org"
  :ensure t
  :config (setq org-log-done t))

(use-package "org-bullets"
  :ensure t
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package "ox-twbs"
  :ensure t)

(use-package "ispell"
  :ensure t
  :config
  (add-to-list 'ispell-local-dictionary-alist '("pl_PL"
                                                "[[:alpha:]]"
                                                "[^[:alpha:]]"
                                                "[']"
                                                t
                                                ("-d" "pl_PL")
                                                nil
                                                utf-8))

  (setq ispell-program-name "hunspell"
        ispell-dictionary   "pl_PL"))

(use-package "flyspell-popup"
  :ensure t)

(flyspell-mode t)

(use-package "auto-yasnippet"
  :ensure t)

(use-package "aggressive-indent"
  :ensure t
  :config (global-aggressive-indent-mode 1))

(use-package "indent-guide"
  :ensure t
  :config (indent-guide-global-mode))

(use-package "buffer-move"
  :ensure t
  :config
  (global-set-key (kbd "<C-M-up>")     'buf-move-up)
  (global-set-key (kbd "<C-M-down>")   'buf-move-down)
  (global-set-key (kbd "<C-M-left>")   'buf-move-left)
  (global-set-key (kbd "<C-M-right>")  'buf-move-right))


(use-package "cheatsheet"
  :ensure t
  :config (global-set-key (kbd "C-c ?") 'cheatsheet-show))

(use-package "alpha"
  :ensure t
  :config
  (set-frame-parameter (selected-frame) 'alpha '(85 . 50))
  (add-to-list 'default-frame-alist '(alpha . (85 . 50))))

(use-package "latex-preview-pane"
  :ensure t)

(use-package "undo-tree"
  :ensure t)

(use-package "rainbow-delimiters"
  :ensure t)

(use-package "focus"
  :ensure t)

(use-package "helm-google"
  :ensure t)

(use-package "git-gutter-fringe+"
  :ensure t)

(use-package "xref-js2"
  :ensure t)

(use-package "helm-css-scss"
  :ensure t)

(use-package "magit"
  :ensure t)

(org-babel-load-file "~/.emacs.d/cheatsheet.org")
