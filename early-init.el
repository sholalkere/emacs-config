;; disable package.el
(setq package-enable-at-startup nil)

;; remove unnecessary gui elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

;; reduce gc overhead during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-percentage 16777216
		  gc-cons-percentage 0.1)))

;; reduce native comp warnings
(setq warning-minimum-level :error)
