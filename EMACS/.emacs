;;init.el
;;Sometimes auto-installing not working, to investigate
(require 'package)
(push '("marmalade" . "http://marmalade-repo.org/packages/")
      package-archives )
(push '("melpa" . "http://melpa.milkbox.net/packages/")
      package-archives)

(package-initialize)

(setq package-list '(auto-complete
                     autopair
                     multiple-cursors
                     web-mode
                     yasnippet
                     company
                     project-explorer
                     sqlup-mode
                     expand-region
                     monokai-theme
                     rainbow-mode ;; Color hexcodes
                     atom-one-dark-theme
                     aurora-theme
                     latex-preview-pane
                     flyspell-popup
                     smex ;; ido-mode for M-x
                     js2-mode ;; better mode for javascript
                     emmet-mode
                     ;; sublime-themes
                     auto-yasnippet
                     aggressive-indent ;; auto indent
                     indent-guide ;; crazy indent line
                     impatient-mode ;; html live reload
                     pastelmac-theme
                     mark-multiple
                     ;; Uncomment if planning to start adventure with clojure
                     ;; cider
                     ;; clojure-cheatsheet
                     ;; clojure-snippets
                     ;; paredit
                     smart-mode-line
                     ws-butler
                     undo-tree
                     solarized-theme
                     rainbow-delimiters
                     switch-window
                     visual-regexp
                     focus
                     firebelly-theme
                     company-web
                     buffer-move
                     helm-google
                     ))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(setq user-mail-address "kamil.galek@gmail.com"
      user-full-name    "Kamil Gałek")

;;Do not show welcome message
(setq inhibit-startup-message t)
(setq make-backup-files nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

;;Fonts
(set-default-font "Source Code Pro")
(set-face-attribute 'default nil :height 100)

;;Hide all bars
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1) ;;Sometimes it's usefull

;;Linum mode - line numbers
(global-linum-mode 1)
(global-set-key [f7] 'linum-mode)
(setq linum-format " %4d ")

(show-paren-mode 1)

;;Uncomment for line highlighting
(global-hl-line-mode 1)
(global-visual-line-mode 1)

;;Smart mode line
;;(setq sml/no-confirm-load-theme t)
;;(sml/setup)
;;(smart-mode-line-enable )

;;Theme load
(load-theme 'solarized-dark t)

(global-set-key (kbd "C-x k") 'kill-buffer-and-window)

;;Trim white space at the end of line
(ws-butler-global-mode)

(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)

(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;Classic
(require 'ido)
(ido-mode t)

;;Ido like for M-x
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(require 'autopair)
(autopair-global-mode)

;; Disabled when company mode is enabled
;;(require 'auto-complete)
;;(global-auto-complete-mode 1)

;;Kill multiple-cursors mode with C-g
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-n") 'mc/mark-next-like-this)

;;Better buffer menu
(global-set-key (kbd "C-x C-b") 'buffer-menu)

(require 'project-explorer)
(global-set-key [f8] 'project-explorer-toggle)

(require 'emmet-mode)
(global-set-key (kbd "C-e") 'emmet-expand-line)

;;Web mode
(require 'web-mode)
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

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook 'ac-js2-mode)

(add-hook 'css-mode-hook 'rainbow-mode)

(require 'yasnippet)
(yas-global-mode 1)
(global-set-key (kbd "C-q") 'company-yasnippet)
(global-set-key (kbd "C-c C-s") 'yas-insert-snippet)

(add-hook 'sql-mode-hook 'sqlup-mode)

(require 'company)
(global-company-mode t)
(global-set-key (kbd "C-SPC") 'company-complete-common)
(add-to-list 'company-backends 'company-web-html)

(require 'org)
(setq org-log-done t)

(require 'ispell)
(flyspell-mode t)
(define-key flyspell-mode-map (kbd "C-t") 'flyspell-popup-correct)

(add-to-list 'ispell-local-dictionary-alist '("pl_PL"
                                              "[[:alpha:]]"
                                              "[^[:alpha:]]"
                                              "[']"
                                              t
                                              ("-d" "pl_PL")
                                              nil
                                              utf-8))

(setq ispell-program-name "hunspell"
      ispell-dictionary   "pl_PL")

(require 'auto-yasnippet)
(global-aggressive-indent-mode 1)

(require 'indent-guide)
(indent-guide-global-mode)

(require 'impatient-mode)

(global-set-key (kbd "<C-M-up>")     'buf-move-up)
(global-set-key (kbd "<C-M-down>")   'buf-move-down)
(global-set-key (kbd "<C-M-left>")   'buf-move-left)
(global-set-key (kbd "<C-M-right>")  'buf-move-right)

(global-set-key (kbd "C-x ;") 'comment-line)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helm-google evil sr-speedbar zenburn-theme zenburn ws-butler web-mode visual-regexp undo-tree switch-window sublime-themes sqlup-mode solarized-theme smex smart-mode-line reykjavik-theme rainbow-mode rainbow-delimiters project-explorer powerline pastelmac-theme paredit oceanic-theme multiple-cursors monokai-theme minimap meacupla-theme markdown-mode mark-multiple latex-preview-pane js3-mode irony indent-guide impatient-mode heroku-theme focus flyspell-popup firebelly-theme expand-region emmet-mode darcula-theme company-web color-theme-sanityinc-tomorrow clues-theme clojure-snippets clojure-cheatsheet buffer-move autopair auto-yasnippet auto-complete aurora-theme atom-one-dark-theme atom-dark-theme ample-zen-theme aggressive-indent ac-js2))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
