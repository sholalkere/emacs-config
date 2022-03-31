;; straight bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use use-package with straight
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; start up maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; ensure custom settings dont clutter init.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

;; load literate config
(org-babel-load-file (expand-file-name "~/.emacs.d/sh-emacs.org"))
