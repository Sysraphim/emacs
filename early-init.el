;; Keyboard-centric user interface
(setq inhibit-startup-message t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq native-comp-async-report-warnings-errors nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq package-enable-at-startup nil)
