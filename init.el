;; -*- lexical-binding: t; -*-
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq package-archives
  '(("melpa" . "https://melpa.org/packages/")
    ("org" . "https://orgmode.org/elpa/")
    ("elpa" . "https://elpa.gnu.org/packages/")))
(package-refresh-contents)
(package-initialize)

;; auto install declared dependencies
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package exec-path-from-shell)
(setq exec-path-from-shell-check-startup-files nil)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;(treesit-library-abi-version)

(use-package emacs
  :bind (([remap list-buffers] . ibuffer)
	 ("M-s-<left>" . previous-buffer)
	 ("M-s-<right>" . next-buffer)
	 ("M-<f7>" . xref-find-referencess)
	 ("s-<backspace>" . kill-whole-line)
	 ("s-<return>" . (lambda() (interactive)
			   (move-end-of-line nil)
			   (electric-newline-and-maybe-indent)))
	 ("<f5>" . deadgrep)
	 ("s-b" . xref-find-definitions)
	 ("M-<f7>" . xref-find-references))
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  :config
  (setq inhibit-startup-screen t)
  (setq require-final-newline t)
  (setq delete-selection-mode t)
  (setq xref-search-program 'ripgrep)
  (setq major-mode-remap-alist
	'((rust-ts-mode . rust-mode)
	  (js2-mode . js-ts-mode)))
  (setq backup-directory-alist '(("." . "~/.saves"))))

;; Indents
;; (setq-default tab-width 4)
;; (add-to-list 'auto-mode-alist '("package.json" . (lambda() () (setq tab-width 2))))

(use-package posframe)

(use-package vertico-posframe
  :init
  (vertico-posframe-mode 1))


;; Remove the border
(setq modus-themes-common-palette-overrides
      '((border-mode-line-active unspecified)
	(border-mode-line-inactive unspecified)))

(load-theme 'modus-vivendi-tinted t)

(set-frame-font "JetBrains Mono 14" nil t)
(global-display-line-numbers-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(global-hl-line-mode 1)
(electric-pair-mode 1)

(which-key-mode 1)
(which-key-setup-side-window-right)
;; switch windows by S-arrow
(windmove-default-keybindings 'meta)

(use-package golden-ratio)
(golden-ratio-mode 1)

;; after run:
;;   - M-x all-the-icons-install-fonts
(use-package all-the-icons
  :if (display-graphic-p))


(use-package dashboard
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-show-shortcuts nil)
  (dashboard-set-heading-icons t)
  (dashboard-icon-type 'all-the-icons)
  (dashboard-set-file-icons t)
  (dashboard-items '(
		     (projects . 20)
		     (recents . 10)
		     (bookmarks . 50)
		     ))
  (dashboard-item-generators '(
			       (projects . dashboard-insert-projects)
			       (recents . dashboard-insert-recents)
			       (bookmarks . dashboard-insert-bookmarks)
			       ))
  :config
  (dashboard-setup-startup-hook))


(use-package flycheck)

(use-package lsp-ui)
(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

;; modular completion framework
(use-package company
  :config
  (setq company-idle-delay 0))

(use-package avy)

(use-package deadgrep)

;; reasons:
;;   - base tool integration: cargo, clippy, rustfmt
;;   - syntax highlighting
;; treesitter-derive requires preinstalls
;;   - goes to https://github.com/casouri/tree-sitter-module/releases and download last release
;;   - copy all libs to ~/.emacs/tree-sitter/ (or some shared, fe /usr/loca/lib)
;;   - if you get modal that mac os cant open dylib goes to settings -> security&privacy and allows to open
;; ensure done:
;;   - rustup component add rust-analyzer
(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t)
  :config
  (setq lsp-rust-analyzer-binding-mode-hints t)
  :custom
  (rust-format-on-save t))


(use-package lsp-mode
  :config
  (setq lsp-inlay-hint-enable t)
  :hook
  (rust-mode . lsp)
  (typescript-ts-mode . lsp)
  (js-ts-mode . lsp)
  (web-mode . lsp)
  :commands lsp)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


;; if node was installed via nvm
(let ((node-bin
       (string-replace "/node\n" ""
                       (shell-command-to-string ". $HOME/.nvm/nvm.sh && nvm which current"))))
  (add-to-list 'exec-path node-bin)
  (setenv "PATH"
          (concat node-bin ":" (getenv "PATH"))))


(use-package vertico
    :init
    (vertico-mode))

;; Persist history over emacs restarts
(use-package savehist
:init
(savehist-mode))


(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
