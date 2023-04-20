(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq use-package-always-ensure t
      straight-use-package-by-default t)


(use-package helm
  :init
  (helm-mode)

  :bind (("M-x" . helm-M-x)
	 ("C-x b" . helm-mini)
	 ([remap apropos] . helm-apropos)
	 ([remap apropos]                   . helm-apropos)
	 ([remap find-library]              . helm-locate-library)
	 ([remap bookmark-jump]             . helm-bookmarks)
	 ([remap execute-extended-command]  . helm-M-x)
	 ([remap find-file]                 . helm-find-files)
	 ([remap ibuffer-find-file]         . helm-find-files)
	 ([remap locate]                    . helm-locate)
	 ([remap imenu]                     . helm-semantic-or-imenu)
	 ([remap noop-show-kill-ring]       . helm-show-kill-ring)
	 ([remap switch-to-buffer]          . helm-buffers-list)
	 ([remap projectile-recentf]        . helm-projectile-recentf)
	 ([remap projectile-switch-project] . helm-projectile-switch-project)
	 ([remap projectile-switch-to-buffer] . helm-projectile-switch-to-buffer)
	 ([remap recentf-open-files]        . helm-recentf)
	 ([remap yank-pop]                  . helm-show-kill-ring))

  :config
  (global-unset-key (kbd "C-x c"))
  (global-set-key (kbd "C-c h") #'helm-command-prefix))

(use-package helm-projectile)

(use-package company
  :custom (company-minimum-prefix-length 1)
  (company-idle-delay 0)

  :config (global-company-mode))

(use-package modus-themes
  :config (load-theme 'modus-operandi-tinted t))

;; General Settings
(setq ring-bell-function 'ignore) ;; Disable the blupp

(set-frame-font "Fira Code-13")

(recentf-mode)

(use-package which-key
  :custom (which-key-idle-delay 0.0)
  :config (which-key-mode))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Nix Integration
(use-package nix-mode)

(use-package envrc
  :bind (:map envrc-mode-map
	      ("C-c e" . envrc-command-map))
  :init (envrc-global-mode))

;; Org Mode
(use-package org
  :bind (("C-c n a" . org-agenda)
	 ("C-c n c" . org-capture))
  
  :custom ((org-agenda-files '("~/Nextcloud/Sync/org/agenda/gtd.org"
			       "~/Nextcloud/Sync/org/agenda/tickler.org"
			       "~/Nextcloud/Sync/org/agenda/inbox.org"))

	   (org-capture-templates '(("t" "Todo [inbox]" entry
				     (file+headline "~/Nextcloud/Sync/org/agenda/inbox.org" "Tasks")
				     "* TODO %i%?")
				    ("T" "Tickler" entry
				     (file+headline "~/Nextcloud/Sync/org/agenda/tickler.org" "Tickler")
				     "* %? \n %^t")))

	   (org-refile-targets '(("~/Nextcloud/Sync/org/agenda/gtd.org" :maxlevel . 3)
				("~/Nextcloud/Sync/org/agenda/someday.org" :level . 1)
				("~/Nextcloud/Sync/org/agenda/tickler.org" :maxlevel . 2)))

	   (org-default-notes-file "~/Nextcloud/Sync/org/agenda/notes.org")
	   (org-log-into-drawer t)
	   (org-return-follows-link t)))

;; IDE Setup
(use-package lsp-mode
  :custom
  (setq lsp-idle-delay 0.500) ;; LSP performance tuning
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (lsp-keymap-prefix "C-c C-l"))

(use-package flycheck)

(use-package scala-mode
  :hook (scala-mode . lsp-deferred))

(use-package lsp-metals
  :custom
  (lsp-metals-server-command "metals -Dmetals.client=emacs")
  (lsp-metals-super-method-lenses-enabled t)
  (lsp-metals-show-inferred-type t)
  (lsp-metals-show-implicit-arguments t)
  (lsp-metals-show-implicit-conversions-and-classes t))

(use-package projectile
  :bind ("C-c p" . projectile-command-map)
  :config (projectile-mode))

(use-package magit)

;; Zettelkasten
(use-package denote
  :bind (("C-c d d" . denote)
	 ("C-c d i" . denote-insert-link)
	 ("C-c d I" . denote-link-insert-links-matching-regexp)))