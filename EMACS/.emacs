;;init.el
;;Sometimes auto-installing not working, to investigate
(require 'package)

;; (push '("marmalade" . "http://marmalade-repo.org/packages/")
;;       package-archives )
;; (push '("melpa" . "http://melpa.milkbox.net/packages/")
;;       package-archives)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/")
             t)

(add-to-list 'package-archives
             ''("marmalade" . "http://marmalade-repo.org/packages/")
             t)

(package-initialize)

(defvar package-list '(auto-complete
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
                       cheatsheet
                       org-bullets
                       ox-twbs
                       alpha
                       column-enforce-mode
                       ac-html-bootstrap
                       ))

;; (dolist (package package-list)
;;   (unless (package-installed-p package)
;;     (package-install package)))

(mapc (lambda (p)
        (package-install p))
      package-list)

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
;; (global-linum-mode 1)
(global-set-key [f7] 'linum-mode)
(setq linum-format " %4d ")

(show-paren-mode 1)
(column-number-mode 1)
(column-enforce-mode 1)
(set-default 'cursor-type 'hbar)
(setq fringes-outside-margins t)

;;Uncomment for line highlighting
(global-hl-line-mode 1)
(global-visual-line-mode 1)

;;Theme load
(load-theme 'spolsky t)

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

(add-hook 'web-mode-hook 'company-web-bootstrap+)
(add-hook 'web-mode-hook 'company-web-fa+)

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
(add-to-list 'company-backends 'company-web-bootstrap+)

(require 'org)
(setq org-log-done t)

(require 'ispell)
(flyspell-mode t)

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

(require 'cheatsheet)
(global-set-key (kbd "C-c ?") 'cheatsheet-show)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'alpha)
(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist '(alpha . (85 . 50)))


(org-babel-load-file "~/.emacs.d/cheatsheet.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3eb93cd9a0da0f3e86b5d932ac0e3b5f0f50de7a0b805d4eb1f67782e9eb67a4" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "06dbcfac3705aaaa79e1a3264c6fd44ef0cf86ef5ed67930e4007e63a8c1e8ee" "ef479623c75026d8ba1de98a8cb74198f6f3eedc6fca509990ac2559ba830675" default)))
 '(package-selected-packages
   (quote
    (airline-themes powerline-evil doom-themes ac-html-bootstrap ac-html undo-tree js2-mode company yasnippet zenburn-theme zenburn ws-butler web-mode visual-regexp switch-window sublime-themes sr-speedbar sqlup-mode solarized-theme smex smart-mode-line reykjavik-theme rainbow-mode rainbow-delimiters project-explorer powerline pastelmac-theme paredit ox-twbs org-bullets oceanic-theme multiple-cursors monokai-theme minimap meacupla-theme markdown-mode mark-multiple latex-preview-pane js3-mode irony indent-guide impatient-mode heroku-theme helm-google focus flyspell-popup firebelly-theme expand-region evil emmet-mode darcula-theme company-web column-enforce-mode color-theme-sanityinc-tomorrow clues-theme clojure-snippets clojure-cheatsheet cheatsheet buffer-move autopair auto-yasnippet auto-complete aurora-theme atom-one-dark-theme atom-dark-theme ample-zen-theme alpha aggressive-indent ac-js2))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
