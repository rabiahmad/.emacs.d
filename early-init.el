

;; Issues with Windows 11 not displaying icons correctly
;; https://github.com/emacs-dashboard/emacs-dashboard/issues/471
(set-language-environment "UTF-8")  ; char-displayable-p returns 'unicode
(set-language-environment "English")  ; char-displayable-p returns nil
(prefer-coding-system 'utf-8)
