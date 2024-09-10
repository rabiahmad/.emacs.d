;; -*- lexical-binding: t; -*-

;; DEBUGGING
(setq debug-on-error nil)

;; Set encoding
(set-language-environment "UTF-8")

;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)

;; Set the right directory to store the native comp cache
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

;; Load Org mode
(require 'org)

;; Function to automatically tangle the README.org file on file save
(defun my-tangle-config ()
  "Tangle the README.org file to produce config.el."
  (when (string= (buffer-file-name) (expand-file-name "README.org" user-emacs-directory))
    (org-babel-tangle)
    (message "Tangled %s" (buffer-file-name))))

;; Add the function to after-save-hook
(add-hook 'after-save-hook
          (lambda ()
            (unless (string= (buffer-file-name) (expand-file-name "README.org" user-emacs-directory))
              (my-tangle-config))))

;; Initial tangle
(org-babel-tangle-file (expand-file-name "README.org" user-emacs-directory))

;; Load the tangled configuration from config.el if it exists
(let ((config-file (expand-file-name "config.el" user-emacs-directory)))
  (if (file-exists-p config-file)
      (load-file config-file)
    (message "Config file %s does not exist" config-file)))
