;; Disable the annoying bell sound
(setq visible-bell 1)

;; Save ~ files and other backups all together
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; How many of the newest versions to keep
      kept-old-versions 5)   ; And how many of the old

(scroll-bar-mode -1)     ; Disable scroll bar
(tool-bar-mode -1)       ; Disable toolbar
(tooltip-mode -1)        ; Disable tooltips
(set-fringe-mode 10)     ; Give more breathing room

(set-frame-parameter nil 'alpha-background 100) ; For current frame, transparency
(add-to-list 'default-frame-alist '(alpha-background . 100)) ; For all new frames henceforth

(menu-bar-mode -1) ; Disable the menu bar

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("gnu-devel" . "https://elpa.gnu.org/devel/")))

(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platform
(unless (package-installed-p 'use-package) ; use-package not installed by default
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode) ; Allow display of line number
(global-display-line-numbers-mode t) ; Activate display of line number

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
                eshell-mode-hook
                shell-mode-hook
                vterm-mode-hook
		treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(dolist (mode '(org-mode-hook
                python-mode-hook
                c-mode-hook
                c++-mode-hook))
  (add-hook mode (lambda () (visual-line-mode 1))))


(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-x f" . counsel-find-file)))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done))
  :config
  (ivy-mode 1))


;;; WINDOW MANAGEMENT
(require 'windmove)

;;;###autoload
(defun buf-move-up ()
  "Swap the current buffer and the buffer above the split.
If there is no split, ie now window above the current one, an
error is signaled."
;;  "Switches between the current buffer, and the buffer above the
;;  split, if possible."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'up))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No window above this one")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun buf-move-down ()
"Swap the current buffer and the buffer under the split.
If there is no split, ie now window under the current one, an
error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'down))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (or (null other-win) 
            (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-win))))
        (error "No window under this one")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun buf-move-left ()
"Swap the current buffer and the buffer on the left of the split.
If there is no split, ie now window on the left of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'left))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No left split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun buf-move-right ()
"Swap the current buffer and the buffer on the right of the split.
If there is no split, ie now window on the right of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'right))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No right split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))


;;; THEMES
(use-package doom-themes
  ;;:ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))



(use-package all-the-icons
  :if (display-graphic-p))


(use-package nerd-icons)


(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode)
  :config
  (setq dired-listing-switches "-agho --group-directories-first"
	dired-omit-files "^\\.[^.].*"
	dired-omit-verbose nil
	dired-dwim-target t ; Copy and move files netween dired buffers
	dired-recursive-copies 'always ; "always" means no asking
	dired-recursive-deletes 'top   ; "top" means ask once for top level directory
	dired-ls-F-marks-symlinks t ; -F marks links with @
	dired-hide-details-hide-symlink-targets nil
	auto-save-list-file-prefix nil ; not create directory .emacs.d/auto-save-list
	;; Auto refresh dired, but be quiet about it
	global-auto-revert-non-file-buffers t
	wdired-allow-to-change-permissions t
	auto-revert-verbose nil
	auto-revert-interval 1
	delete-by-moving-to-trash t))


;; This changes the modeline bar at the bottom of the screen
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 35      ;; sets modeline height
        doom-modeline-bar-width 5    ;; sets right bar width
        doom-modeline-persp-name t   ;; adds perspective name to modeline
        doom-modeline-persp-icon t   ;; adds folder icon next to persp name
	inhibit-compacting-font-caches t  ;; fix lagging issue
	))

;; ( use-package neotree
;;   :config
;;   (setq neo-smart-open t
;;         neo-show-hidden-files t
;;         neo-window-width 55
;;         neo-window-fixed-size nil
;;         inhibit-compacting-font-caches t
;;         projectile-switch-project-action 'neotree-projectile-action) 
;;         ;; truncate long file names in neotree
;;         (add-hook 'neo-after-create-hook
;;            #'(lambda (_)
;;                (with-current-buffer (get-buffer neo-buffer-name)
;;                  (setq truncate-lines t)
;;                  (setq word-wrap nil)
;;                  (make-local-variable 'auto-hscroll-mode)
;;                  (setq auto-hscroll-mode nil)))))


;; (setq neo-theme (if (display-graphic-p) 'icons 'a
;;		    rrow))


;; ----------------------------------------------
;;     TREEMACS
;; ----------------------------------------------

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

;; (use-package treemacs-projectile
;;   :after (treemacs projectile)
;;   :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

;; (treemacs-start-on-boot)

;; ----------- END TREEMACS ------------ ;;




;; Make parentheses different colors to easily tell how they close
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Gives more useful completion when you start typing a command
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

;; Add to to ivy to provide command descriptions
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; Makes help interface more contextual
(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


(use-package org
  :config
  (setq org-ellipsis " ▾")
  (setq org-log-done t))

(use-package toc-org
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(electric-indent-mode -1)

(require 'org-tempo)

(use-package rainbow-mode
  :hook 
  ((org-mode prog-mode) . rainbow-mode))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Notes")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-db-autosync-mode))

(setq org-agenda-files (list "~/Notes/work.org"
                             "~/Notes/home.org"))

(use-package csv-mode
  :ensure t
  :hook (csv-mode . csv-align-mode))

;; Git porcelain (porcelain = make it nice)
;; Main control is C-x g
(use-package magit
  :ensure t
  :config
  (setq magit-save-repository-buffers nil))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :custom
  ;; M-<num> to select an option according to its number.
  (company-show-numbers t)
  ;; Only 2 letters required for completion to activate.
  (company-minimum-prefix-length 2)
  ;; Do not downcase completions by default.
  (company-dabbrev-downcase nil)
  ;; Even if I write something with the wrong case,
  ;; provide the correct casing.
  (company-dabbrev-ignore-case t)
  ;; Company completion wait
  (company-idle-delay 0.2)
  ;; No company-mode in shell & eshell
  (company-global-modes '(not eshell-mode shell-mode)))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) . ("clangd"))))

;; For some reason can't add this to use-package above
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

;; Don't blow out the minibuffer with company
(setq eldoc-echo-area-use-multiline-p nil)

;; Some extra python fluff
(add-hook 'python-mode-hook (lambda () (setq fill-column 120)))


;;; FONT CUSTOMISATIONS
;; Set the font everywhere
(set-frame-font "JetBrains Mono-12" nil t)

;; Check if Nerd Font is installed and avoid installation prompt
(if (member "JetBrains Mono" (font-family-list))
    (message "Nerd Font is installed")
  (message "Nerd Font is NOT installed"))

;; Set the unicode font
(setq doom-unicode-font (font-spec :family "JetBrains Mono" :size 11))

(set-face-attribute 'default nil :height 120)  ;; Adjust font size to 12 points


;;; DASHBOARD SETTINGS
(use-package dashboard
  :ensure t
  :init
  (setq initial-buffer-choice 'dashboard-open)
  ;; possible values: 'official, 'logo, integers (1, 2, 3, 4)
  (setq dashboard-startup-banner 2)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  ;; choose which sections to show and how many items per section
  (setq dashboard-items '((recents   . 5)
			  (projects  . 5)
			  (agenda    . 5)
			  (bookmarks . 5)
			  (registers . 5)))
  ;; customize which widgets to display in order
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                    ;; dashboard-insert-newline
                                    ;; dashboard-insert-banner-title
                                    ;; dashboard-insert-newline
                                    dashboard-insert-navigator
                                    dashboard-insert-newline
                                    dashboard-insert-init-info
                                    dashboard-insert-items
                                    dashboard-insert-newline
                                    dashboard-insert-footer))

 ;; customise the shortcuts for each heading on the dashboard
  (setq dashboard-item-shortcuts '((recents   . "r")
				   (projects  . "p")
				   (agenda    . "a")
				   (bookmarks . "m")
				   (registers . "e")))

  (setq dashboard-item-names '(("Agenda for the coming week:" . "Agenda:")))

  :config
  (dashboard-setup-startup-hook)
  )

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

(use-package page-break-lines)

;;; OTHER USEFUL FUNCS
(defun reload-init-file ()
  (interactive)
  (load-file user-init-file)
  (load-file user-init-file))

;;; EVIL MODE
(unless (package-installed-p 'evil)
  (package-install 'evil))

(require 'evil)
(evil-mode 1)


;;; GLOBAL KEY BINDINGS
(use-package general)

(define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)


(general-define-key
 "C-M-j" 'counsel-switch-buffer
 "C-x r C-f" 'counsel-recentf
 "C-M-i" '(lambda () (interactive) (find-file user-init-file))
 "C-c l" 'org-store-link
 "C-c a" 'org-agenda
 "C-c C-/" 'comment-or-uncomment-region
)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(treemacs-tab-bar treemacs-persp which-key vterm treemacs-magit treemacs-icons-dired treemacs-evil toc-org rainbow-mode rainbow-delimiters pyenv-mode poetry pet page-break-lines org-roam org-bullets nerd-icons-dired neotree ivy-rich helpful general exec-path-from-shell ef-themes doom-themes doom-modeline dashboard csv-mode counsel company-box command-log-mode all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
