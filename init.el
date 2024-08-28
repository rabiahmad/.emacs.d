;; Load Org mode and tangle the README.org file to produce init.el
(require 'org)
(org-babel-load-file (expand-file-name "README.org" user-emacs-directory))
