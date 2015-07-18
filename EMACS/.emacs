;;init.el
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
		     ace-jump-mode
		     sqlup-mode
         expand-region
         jabber
         monokai-theme))

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
(menu-bar-mode -1)

(load-theme 'monokai t)
(set-default-font "Source Code Pro")

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'minimap)
(setq minimap-window-location 'right)
(global-set-key [f9] 'minimap-toggle)

(require 'ido)
(ido-mode t)

(require 'autopair)
(autopair-global-mode)

(require 'multiple-cursors)
    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    (global-set-key (kbd "C-n") 'mc/mark-next-like-this)

;;Better buffer menu
(global-set-key (kbd "C-x C-b") 'buffer-menu)

(require 'project-explorer)
(global-set-key [f8] 'project-explorer-toggle)

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

(require 'ace-jump-mode)
    (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(add-hook 'sql-mode-hook 'sqlup-mode)

;;Eclim
;;Installation instruction can be found in other file
(require 'eclim)
(global-eclim-mode)

(require 'eclimd)
(custom-set-variables
 '(eclim-eclipse-dirs '("/opt/eclipse"))
 '(eclim-executable "/opt/eclipse/eclim"))


(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

(require 'company)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(global-company-mode t)
(global-set-key (kbd "C-SPC") 'company-complete-common)

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-agenda-files (list "~/Dropbox/org-mode/praca_inzynierska/main.org"
                             "~/Dropbox/org-mode/studia/wat.org"))

(require 'jabber)
(setq jabber-account-list '(
                            ("kamil.galek@gmail.com"
                             (:network-server . "talk.google.com")
                             (:connection-type . ssl)
                             (:port . 5222))
                            ))
(custom-set-variables
 '(jabber-auto-reconnect t)
 '(jabber-avatar-verbose nil)
 '(jabber-chat-buffer-format "*-jabber-%n-*")
 '(jabber-history-enabled f)
 '(jabber-mode-line-mode t)
 '(jabber-roster-line-format "%-30n %-8s")
 '(jabber-vcard-avatars-retrieve nil))
