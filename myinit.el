(server-start)
(setq x-select-enable-clipboard t)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
	backup-by-copying      t  ; Don't de-link hard links
	version-control        t  ; Use version numbers on backups
	delete-old-versions    t  ; Automatically delete excess backups:
	kept-new-versions      20 ; how many of the newest versions to keep
	kept-old-versions      5) ; and how many of the old
 (set-face-attribute 'default nil :font "Iosevka" :height 140)

(use-package doom-themes
  :ensure t
  :init
  (load-theme 'doom-vibrant t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package org-bullets
  :ensure t
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

(use-package all-the-icons
  :ensure t) ;; remember to all-the-icons-install-font

(use-package ligature
  :straight (ligature :type git :host github :repo "mickeynp/ligature.el")
  ;;:load-path "load path"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
				       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
				       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
				       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
				       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
				       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
				       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
				       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
				       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
				       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
				       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
				       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
				       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents . 5)
			  (bookmarks . 5)
			  (projects . 5))))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window))

(use-package company
  :ensure t
  :hook
  (after-init . global-company-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package counsel
  :ensure t)

(use-package swiper
  :ensure try
    :config
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-load-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    )

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t)

(use-package undo-tree
  :ensure
  :init
  (global-undo-tree-mode))

(use-package tex-mode
  :ensure auctex
  :config
  (setq TeX-save-query nil)
  )

(use-package company-auctex
  :ensure t
  :config
  (company-auctex-init))

;; (use-package pdf-tools
;;   :ensure t
;;   :config
;;   (pdf-tools-install))

(use-package magit
  :ensure t)

(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode +1))

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
	 (tuareg-mode . lsp)
	 ;; if you want which-key integration
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(defun ocamllsp-setup () 
  (lsp-register-client
   (make-lsp-client
    :new-connection
    (lsp-stdio-connection '("opam" "exec" "--" "ocamllsp"))
    :major-modes '(tuareg-mode)
    :priority -6
    :server-id 'ocamllsp)))

(use-package merlin
  :ensure t)
(use-package merlin-company
  :ensure t)
(use-package merlin-iedit
  :ensure t)
(use-package merlin-ac
  :ensure t)

(use-package ocamlformat
  :ensure t)

(defun ocamlformat-before-save ()
  "Add this to .emacs to run ocamlformat on the current buffer when saving:
  \(add-hook 'before-save-hook 'ocamlformat-before-save)."
  (interactive)
  (when (eq major-mode 'tuareg-mode) (ocamlformat)))

(use-package tuareg
  :ensure t
  :config
  (with-eval-after-load 'company (add-to-list 'company-backends 'merlin-company-backend))
  (with-eval-after-load "lsp-mode"
    (setq lsp-enabled-clients '(ocamllsp))
    (ocamllsp-setup))
  :hook
  (tuareg-mode . lsp)
  (tuareg-mode . merlin-mode)
  (before-save . ocamlformat-before-save))

(use-package vterm
  :ensure t)

(use-package multi-vterm
  :ensure t
  :bind(("C-x v n" . multi-vterm))
  )

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (global-set-key (kbd "C-c t") 'neotree-toggle))
