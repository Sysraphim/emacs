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

(use-package helm-lsp)

(use-package helm-projectile
  :config (helm-projectile-on))

(use-package helm-descbinds
  :config (helm-descbinds-mode))

(use-package helm-org
  :bind (("C-c n A" . helm-org-agenda-files-headings))
  :init
  (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
  (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags)))

(use-package company
  :custom (company-minimum-prefix-length 1)
  (company-idle-delay 0)

  :config (global-company-mode))

(use-package doom-themes
  :config (load-theme 'doom-dark+ t))

;; Prettier Modeline
(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

;; Menu for minor modes
(use-package minions
  :config (minions-mode))

;; General Settings
(setq ring-bell-function 'ignore) ;; Disable the blupp

(set-frame-font "Fira Code-13")

(recentf-mode)

(use-package which-key
  :custom (which-key-idle-delay 0.8)
  :config (which-key-mode))

;; Backup Settings
(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t   ;; Copy all files, don't rename them.
      vc-make-backup-files t) ;; Also backup files, that are in vc

(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq create-lockfiles nil)

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
  :straight (:type built-in)
  :hook (org-mode . org-indent-mode)

  :bind (("C-c n a" . org-agenda)
	 ("C-c n c" . org-capture))
  
  :custom ((org-agenda-files '("~/Nextcloud/Sync/org/agenda/gtd.org"
			       "~/Nextcloud/Sync/org/agenda/tickler.org"
			       "~/Nextcloud/Sync/org/agenda/inbox.org"))

	   (org-capture-templates '(("t" "Todo [inbox]" entry
				     (file+headline "~/Nextcloud/Sync/org/agenda/inbox.org" "Inbox")
				     "* TODO %i%?")

				    ("e" "Zettelkasten" entry
				     (file+headline "~/Nextcloud/Sync/braindump/kleinhirn.org" "Kleinhirn")
				     "* %i%?")

				    ("T" "Tickler" entry
				     (file+headline "~/Nextcloud/Sync/org/agenda/tickler.org" "Tickler")
				     "* %? \n %^t")))

	   (org-refile-targets '(("~/Nextcloud/Sync/org/agenda/gtd.org" :maxlevel . 3)
				 ("~/Nextcloud/Sync/org/agenda/someday.org" :level . 1)
				 ("~/Nextcloud/Sync/org/agenda/tickler.org" :maxlevel . 2)))

	   (org-default-notes-file "~/Nextcloud/Sync/org/agenda/notes.org")
	   (org-log-into-drawer t)
	   (org-return-follows-link t)))

(setq org-todo-keywords
      '((sequence "TODO(q)" "NEXT(w)" "PROJ(e)" "WAITING(r)" "DEFERRED(t)" "DELEGATED(z)" "|" "DONE(d)" "CANCELLED(c)")))

(use-package elfeed
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
        elfeed-show-entry-switch 'display-buffer)
  :bind
  ("C-x w" . elfeed))
(use-package elfeed-org
  :config
  (elfeed-org))

;; IDE Setup
(use-package lsp-mode
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :custom
  (setq lsp-idle-delay 0.500) ;; LSP performance tuning
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (lsp-keymap-prefix "C-x L"))

;; Fix escape codes in compilation buffers
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package lsp-ui)

(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package lsp-treemacs)

(use-package flycheck)


(use-package lsp-pyright)

(use-package python-mode
  :custom (python-indent-guess-indent-offset-verbose nil)
  :hook ((python-mode . lsp-deferred)
	 (python-mode . blacken-mode)
	 (python-mode . pyvenv-mode)
	 (python-mode . pyvenv-tracking-mode)))

(use-package pyvenv
  :custom (pyvenv-default-virtual-env-name "venv"))

(use-package numpydoc)

(use-package blacken)

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
  :bind ("C-x p" . projectile-command-map)
  :config (projectile-mode))

(use-package magit)

(use-package vterm
  :bind ("<f12>" . vterm))

;; Zettelkasten
(defvar zk/directory "/home/mkrause/Nextcloud/Sync/braindump")

(use-package org-roam
  :config
  (setq org-roam-directory (file-truename zk/directory))
  (org-roam-db-autosync-mode)

  (setq org-roam-mode-sections
	(list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              ;; #'org-roam-unlinked-references-section
              ))

  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
		 (display-buffer-in-direction)
		 (direction . right)
		 (window-width . 0.33)
		 (window-height . fit-window-to-buffer)))
  :bind
  (("C-c n r f" . org-roam-node-find)
   ("C-c n r i" . org-roam-node-insert)
   ("C-c n r b" . org-roam-buffer-toggle)))
