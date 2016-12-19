(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/")
             t)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/")
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
                       doom-themes
                       git-gutter-fringe+
                       flycheck
                       xref-js2
                       ))

(mapc (lambda (p)
        (package-install p))
      package-list)

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

;;Linum mode - line numbers
;; (global-linum-mode 1)
(global-set-key [f7] 'linum-mode)
;;(setq linum-format " %4d ")

(show-paren-mode 1)
(column-number-mode 1)
(column-enforce-mode 1)
(set-default 'cursor-type 'hbar)
(setq fringes-outside-margins t)

;;Uncomment for line highlighting
(global-hl-line-mode 1)
(global-visual-line-mode 1)

;;Theme load
(load-theme 'doom-one t)

(global-set-key (kbd "C-x k") 'kill-buffer-and-window)

;;Trim white space at the end of line
(ws-butler-global-mode)

(require 'flycheck)
(global-flycheck-mode)

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
(add-hook 'web-mode-hook 'superword-mode)

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-hook 'js2-mode-hook 'subword-mode)

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
 '(package-selected-packages
   (quote
    (ox-twiki xref-js2 beacon neotree flycheck undo-tree company yasnippet web-mode ws-butler web-mode-edit-element visual-regexp switch-window sublime-themes sqlup-mode sourcerer-theme solarized-theme smex smartparens smart-mode-line rainbow-mode rainbow-delimiters project-explorer pastelmac-theme paredit ox-twbs org-bullets octicons multiple-cursors monokai-theme material-theme mark-multiple latex-preview-pane js2-mode indent-guide impatient-mode helm-google git-gutter-fringe+ git-gutter focus flyspell-popup flycheck-clangcheck firebelly-theme expand-region evil emmet-mode doom-themes company-web column-enforce-mode clojure-snippets clojure-cheatsheet cheatsheet buffer-move autopair auto-yasnippet auto-complete aurora-theme atom-one-dark-theme alpha aggressive-indent ac-html-bootstrap))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
