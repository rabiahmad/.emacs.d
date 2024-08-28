;; -*- lexical-binding: t; -*-

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



;; ;;; This is the actual config file. It is omitted if it doesn't exist so emacs won't refuse to launch.
;; (when (file-readable-p "~/.emacs.d/README.org")
;;   (org-babel-load-file (expand-file-name "README.org")))
