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
                     emacs-eclim
                     company
                     project-explorer
                     minimap
                     ;;ace-jump-mode - nice, but for me useless
                     sqlup-mode
                     expand-region
                     monokai-theme
                     rainbow-mode ;; Color hexcodes
                     atom-one-dark-theme
                     aurora-theme
                     latex-preview-pane
                     flyspell-popup
                     ;;helm - looks nice but I don't like it
                     smex ;; ido-mode for M-x
                     js2-mode ;; better mode for javascript
                     emmet-mode
                     sublime-themes
                     auto-yasnippet
                     aggressive-indent ;; auto indent
                     indent-guide ;; crazy indent line
                     impatient-mode ;; html live reload
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

;;Hide all bars
(scroll-bar-mode -1)
(tool-bar-mode -1)
;; (menu-bar-mode -1) ;;Sometimes it's usefull

(set-face-attribute 'default nil :height 90)

(global-set-key [f7] 'linum-mode)
(setq linum-format " %4d ")

;;Uncomment for line highlighting
;;(global-hl-line-mode 1)

(global-visual-line-mode 1)

(load-theme 'spolsky t)
(set-default-font "Source Code Pro")

(global-set-key (kbd "C-x k") 'kill-buffer-and-window)

(add-hook 'css-mode-hook 'rainbow-mode)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'minimap)
(setq minimap-window-location 'right)
(global-set-key [f10] 'minimap-toggle)

;;Classic
(require 'ido)
(ido-mode t)

;;Ido like for M-x
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;;(require 'helm-config)
;;(global-set-key (kbd "M-x") 'helm-M-x)

(require 'autopair)
(autopair-global-mode)

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
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(require 'yasnippet)
(yas-global-mode 1)
(global-set-key (kbd "C-q") 'company-yasnippet)

;;(require 'ace-jump-mode)
;;    (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(add-hook 'sql-mode-hook 'sqlup-mode)

;;Eclim
;;Installation instruction can be found in other file
(require 'eclim)
(global-eclim-mode)

(require 'eclimd)
(custom-set-variables
 '(eclimd-default-workspace "~/Dev/workspace/"))

(global-set-key [f9] 'eclim-maven-run)

;;(setq help-at-pt-display-when-idle t)
;;(setq help-at-pt-timer-delay 0.1)
;;(help-at-pt-set-timer)

(require 'company)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(global-company-mode t)
(global-set-key (kbd "C-SPC") 'company-complete-common)

(require 'org)
(setq org-log-done t)

(setq org-agenda-files (list "~/Dropbox/org-mode/praca_inzynierska/main.org"
                             "~/Dropbox/org-mode/studia/wat.org"))

;;Set font settings for latex mode
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-latex-sectioning-0-face ((t (:inherit font-latex-sectioning-1-face))))
 '(font-latex-sectioning-1-face ((t (:inherit font-latex-sectioning-2-face))))
 '(font-latex-sectioning-2-face ((t (:inherit font-latex-sectioning-3-face))))
 '(font-latex-sectioning-3-face ((t (:inherit font-latex-sectioning-4-face))))
 '(font-latex-sectioning-4-face ((t (:inherit font-latex-sectioning-5-face))))
 '(font-latex-sectioning-5-face ((t (:foreground "#74CBC4" :weight bold))))
 '(font-latex-slide-title-face ((t (:inherit (variable-pitch font-lock-type-face) :weight bold)))))

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
