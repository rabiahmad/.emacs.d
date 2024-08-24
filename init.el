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

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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
(dolist (mode '(term-mode-hook
                eshell-mode-hook
                shell-mode-hook
                vterm-mode-hook))
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


;;;; WINDOW MANAGEMENT

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


;;;; THEMES

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))



(use-package all-the-icons
  :if (display-graphic-p))


(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "JetBrains Mono")
  :if (display-graphic-p)
  )


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
        doom-modeline-persp-icon t)) ;; adds folder icon next to persp name


(use-package neotree
  :config
  (setq neo-smart-open t
        neo-show-hidden-files t
        neo-window-width 55
        neo-window-fixed-size nil
        inhibit-compacting-font-caches t
        projectile-switch-project-action 'neotree-projectile-action) 
        ;; truncate long file names in neotree
        (add-hook 'neo-after-create-hook
           #'(lambda (_)
               (with-current-buffer (get-buffer neo-buffer-name)
                 (setq truncate-lines t)
                 (setq word-wrap nil)
                 (make-local-variable 'auto-hscroll-mode)
                 (setq auto-hscroll-mode nil)))))


(setq neo-theme (if (display-graphic-p) 'icons 'arrow))


; DASHBOARD SETTINGS
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
  ;; choose which sections to show and how many items per section
  (setq dashboard-items '((recents   . 5)
			  (projects  . 5)
			  (bookmarks . 5)
			  (registers . 5)))
  ;; customize which widgets to display in order
  (setq dashboard-startupify-list '(dashboard-insert-banner
				    dashboard-insert-navigator
				    dashboard-insert-items))
  ;; customise the shortcuts for each heading on the dashboard
  (setq dashboard-item-shortcuts '((recents   . "r")
				   (projects  . "p")
				   (bookmarks . "m")
				   (registers . "e")))
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
				    (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook)
  )

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))


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

;; Allows pet below to find the shell path properly on macos
;; Will this break anything on linux?
(use-package exec-path-from-shell
  :if (memq (window-system) '(mac ns))
  :config (exec-path-from-shell-initialize))

;; Allows eglot to always find your python env when set with pyenv or poetry
;; Config stolen from github page
(use-package pet
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local python-shell-interpreter (pet-executable-find "python")
                          python-shell-virtualenv-root (pet-virtualenv-root))

              (pet-eglot-setup)
              (eglot-ensure)

              (setq-local lsp-jedi-executable-command
                          (pet-executable-find "jedi-language-server")))))

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

;; Set a different font for icons
(setq nerd-icons-font-family "JetBrains Mono")

;; Set the unicode font
(setq doom-unicode-font (font-spec :family "JetBrains Mono" :size 11))

(set-face-attribute 'default nil :height 120)  ;; Adjust font size to 12 points


;;; OTHER USEFUL FUNCS
(defun reload-init-file ()
  (interactive)
  (load-file user-init-file)
  (load-file user-init-file))
