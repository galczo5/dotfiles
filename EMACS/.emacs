(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq vc-follow-symlinks t)
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq indent-line-function 'insert-tab)

(setq-default fringes-outside-margins t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(fringe-mode 20)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(ido-mode t)

(show-paren-mode 1)

(blink-cursor-mode -1)
(set-default 'cursor-type 'box)

(global-hl-line-mode 1)

(global-set-key (kbd "C-x k") 'kill-buffer-and-window)
(global-set-key (kbd "C-x ;") 'comment-line)
(global-set-key (kbd "C-x C-b") 'buffer-menu)

(global-set-key [f7] 'linum-mode)
(setq linum-format " %4d  |  ")

(set-frame-font "Iosevka")
(set-face-attribute 'default nil :height 130)

(use-package "hlinum"
  :ensure t
  :config
  (hlinum-activate)
  (add-hook 'linum-mode-hook (lambda ()
                               (set-face-background 'linum-highlight-face
                                                    (face-background 'default))
                               (set-face-foreground 'linum-highlight-face "#ffffff")))
  (global-linum-mode 1))

(use-package "base16-theme"
  :ensure t)

(defun customize-theme ()
  "my theme customs"
  (set-face-background 'mode-line "#1e1e1e")
  (set-face-attribute 'mode-line nil
                      :box '(:line-width 5 :color "#1e1e1e"))
  (set-face-attribute 'mode-line-inactive nil
                      :box '(:line-width 5 :color "#1e1e1e"))
  (set-face-background 'linum "#1e1e1e")
  (set-face-attribute 'mode-line nil :height 110 :weight 'light)
  (set-face-attribute 'mode-line-inactive nil :height 110 :weight 'light)
  (set-face-background 'vertical-border "#1e1e1e")
  (set-face-foreground 'vertical-border (face-background 'vertical-border))
  (set-face-attribute 'fringe nil :background nil)
  )

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (when (eq (length (frame-list)) 2)
                  (progn (select-frame frame)
                         (load-theme 'base16-twilight t)
                         (customize-theme)))))

  (load-theme 'base16-twilight t)
  (customize-theme))

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

(use-package "emmet-mode"
  :ensure t
  :config (global-set-key (kbd "C-e") 'emmet-expand-line))

(use-package "web-mode"
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))
  :config
  (add-hook 'web-mode-hook 'superword-mode)
  (add-hook 'web-mode-hook 'aggressive-indent-mode))

(use-package "company"
  :ensure t
  :config
  (global-company-mode 1)
  (setq company-idle-delay 0)
  (global-set-key (kbd "C-SPC") 'company-complete-common))

(use-package "yasnippet"
  :ensure t
  :config
  (yas-global-mode t)
  (global-set-key (kbd "C-q") 'company-yasnippet)
  (global-set-key (kbd "C-c C-s") 'yas-insert-snippet))

(use-package "yasnippet-snippets"
  :ensure t)

(use-package "auto-yasnippet"
  :ensure t)

(use-package "org"
  :ensure t
  :config (setq org-log-done t))

(use-package "org-bullets"
  :ensure t
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package "flyspell-popup"
  :ensure t)

(use-package "indent-guide"
  :ensure t
  :config (indent-guide-global-mode))

(use-package "alpha"
  :ensure t
  :config
  (set-frame-parameter (selected-frame) 'alpha '(95 . 95))
  (add-to-list 'default-frame-alist '(alpha . (95 . 95))))

(use-package "latex-preview-pane"
  :ensure t)

(use-package "magit"
  :ensure t)

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

(use-package "editorconfig"
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package "markdown-mode"
  :ensure t)

(use-package "js2-mode"
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :config
  (add-hook 'js2-mode-hook 'ac-js2-mode)
  (add-hook 'js2-mode-hook 'subword-mode))

(use-package "typescript-mode"
  :ensure t)

(use-package "ng2-mode"
  :ensure t)

(use-package "tide"
  :ensure t
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode 1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode 1)
    (tide-hl-identifier-mode 1)
    (company-mode 1))

  (setq company-tooltip-align-annotations t)
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(use-package "vue-mode"
  :ensure t)

(use-package "vue-html-mode"
  :ensure t)
