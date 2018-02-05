(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/")
             t)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/")
             t)

(package-initialize)

(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Personal info
(setq user-mail-address "kamil.galek@gmail.com"
      user-full-name    "Kamil Gałek")

(setq vc-follow-symlinks t)
(setq inhibit-startup-message t)
(setq make-backup-files nil)

(setq-default fringes-outside-margins t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

(set-frame-font "DejaVu Sans Mono")
(set-face-attribute 'default nil :height 120)
(fringe-mode 20)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(show-paren-mode 1)
(column-number-mode 1)

(set-default 'cursor-type 'box)

(global-hl-line-mode 1)
(global-visual-line-mode 1)

(global-set-key (kbd "C-x k") 'kill-buffer-and-window)
(global-set-key (kbd "C-x ;") 'comment-line)
(global-set-key (kbd "C-x C-b") 'buffer-menu)

(global-set-key [f7] 'linum-mode)
(setq linum-format " %4d    ")

(ido-mode t)

(use-package "hlinum"
  :ensure t
  :config
  (hlinum-activate)
  (add-hook 'linum-mode-hook (lambda () (set-face-background 'linum-highlight-face (face-background 'default))
(set-face-foreground 'linum-highlight-face "#ffffff"))))

(use-package "fill-column-indicator"
  :ensure t
  :config
  (setq fci-rule-column 80)
  (setq fci-rule-use-dashes 1))

(use-package "doom-themes"
  :ensure t)

(use-package "badwolf-theme"
  :ensure t)

(use-package "kaolin-theme"
  :ensure t)

(use-package "ample-zen-theme"
  :ensure t)

(use-package "dracula-theme"
  :ensure t)

(use-package "zerodark-theme"
  :ensure t)

(if (daemonp)
    (add-hook 'after-make-frame-functions (lambda (frame)
                        (when (eq (length (frame-list)) 2)
                            (progn
                              (select-frame frame)
                              (load-theme 'zerodark t)
                              (zerodark-setup-modeline-format)
                              (set-face-attribute 'fringe nil :background nil)))))
  (load-theme 'zerodark t)
  (zerodark-setup-modeline-format)
  (set-face-attribute 'fringe nil :background nil))

(use-package "all-the-icons"
  :ensure t)

(use-package "column-enforce-mode"
  :ensure t
  :config (column-enforce-mode 1))

(use-package "aggressive-indent"
  :ensure t)

(use-package "helm-swoop"
  :ensure t
  :config (global-set-key (kbd "C-s") 'helm-swoop))

(use-package "ws-butler"
  :ensure t
  :config (ws-butler-global-mode))

(use-package "flycheck"
  :ensure t)

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
  :config
  ;;(global-set-key [f8] 'project-explorer-toggle)
  )

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
  :config
  (add-hook 'web-mode-hook 'superword-mode)
  (add-hook 'web-mode-hook 'aggressive-indent-mode))

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

(use-package "htmlize"
  :ensure t)

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
  (add-to-list 'ispell-local-dictionary-alist '("en_US"
                                              "[[:alpha:]]"
                                              "[^[:alpha:]]"
                                              "[']"
                                              t
                                              ("-d" "en_US")
                                              nil
                                              iso-8859-1))
  (setq ispell-program-name "hunspell"
        ispell-dictionary   "pl_PL"))

(use-package "flyspell-popup"
  :ensure t)

(use-package "auto-yasnippet"
  :ensure t)

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

(use-package "alpha"
  :ensure t
  :config
  (set-frame-parameter (selected-frame) 'alpha '(95 . 95))
  (add-to-list 'default-frame-alist '(alpha . (95 . 95)))
  )

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

(use-package "git-gutter-fringe"
  :ensure t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
    nil nil 'center)
  (global-git-gutter-mode))

(use-package "helm-css-scss"
  :ensure t)

(use-package "magit"
  :ensure t)

(defun git ()
  (interactive)
  (magit-status))

(use-package "irony"
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)

  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  ;; (add-hook 'c-mode-common-hook 'auto-complete-mode)
  (setq gdb-many-windows t
        gdb-show-main t)

  (add-hook 'c-mode-common-hook 'linum-mode)
  (add-hook 'c-mode-common-hook 'aggressive-indent-mode)

  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-hook 'c-mode-common-hook (lambda ()
                                  (setq flycheck-gcc-language-standard "c++17")))
  (add-hook 'c-mode-common-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook (lambda ()
                             (setq irony-additional-clang-options '("-std=c++17")))))

(use-package "sr-speedbar"
  :ensure t
  :config
  (setq speedbar-show-unknown-files t)
  (setq sr-speedbar-right-side nil)
  (setq sr-speedbar-max-width 35)
  (setq sr-speedbar-width 35)
  (global-set-key [f8] 'sr-speedbar-toggle))

(use-package "srefactor"
  :ensure t
  :config (semantic-mode 1))

(defun header ()
  (interactive)
  (find-file-existing
   (concat (file-name-sans-extension (buffer-file-name)) ".h")))

(defun source ()
  (interactive)
  (find-file-existing
   (concat (file-name-sans-extension (buffer-file-name)) ".cpp")))

(use-package "flycheck-irony"
  :ensure t
  :init
  (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook 'flycheck-irony-setup)))

(use-package "company-irony"
  :ensure t
  :init (add-to-list 'company-backends 'company-irony))

(use-package "company-c-headers"
  :ensure t
  :init (add-to-list 'company-backends 'company-c-headers))

(use-package "google-c-style"
  :ensure t
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))

(use-package "function-args"
  :ensure t
  :config (fa-config-default))

(use-package "smooth-scrolling"
  :ensure t
  :config (smooth-scrolling-mode 1))

(use-package "rust-mode"
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

(use-package "racer"
  :ensure t
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package "jedi"
  :ensure t
  :config (add-hook 'python-mode-hook 'jedi:setup))

(use-package "company-jedi"
  :ensure t
  :config

  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))

  (add-hook 'python-mode-hook 'my/python-mode-hook))
