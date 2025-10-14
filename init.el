;; -*- lexical-binding: t; -*-
;; before:
;; rustup component add rust-analyzer
;; brew install gopls
;;
;;
;;
;; after:
;; M-x all-the-icons-install-fonts (if use all-the-icons)

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

(setq-default tab-width 4)

(use-package emacs
  :bind (([remap list-buffers] . ibuffer)
	 ("M-s-<left>" . previous-buffer)
	 ("M-s-<right>" . next-buffer)
	 ("M-<f7>" . xref-find-referencess)
     ("s-/" . comment-line)
     ("s-d" . duplicate-thing)       
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
      (go-ts-mode . go-mode)   
	  (js2-mode . js-ts-mode)))
  (setq backup-directory-alist '(("." . "~/.saves"))))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package duplicate-thing)

(use-package posframe)

(use-package vertico-posframe
  :init
  (vertico-posframe-mode 1))


;; Remove the border
;; (setq modus-themes-common-palette-overrides
      ;; '((border-mode-line-active unspecified)
	;; (border-mode-line-inactive unspecified)))


(use-package doom-themes
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  (load-theme 'doom-challenger-deep t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

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
(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t)
  :config
  (setq lsp-rust-analyzer-binding-mode-hints t)
  :custom
  (rust-format-on-save t))

;; Golang
(defun go-run ()
      "Run the current Go project."
      (interactive)
      (async-shell-command "go run ."))

;; Golang
(defun go-run-tests ()
      "Run the current Go project tests"
      (interactive)
      (async-shell-command "go test ./..."))

(use-package go-mode
    :bind (
         :map go-mode-map
         ("<f9>" . go-run)
         ("<f10>" . go-run-tests))
    :hook
    (go-mode . gofmt-before-save))

(use-package go-ts-mode
  :bind (
         :map go-ts-mode-map
         ("<f9>" . go-run)
         ("<f10>" . go-run-tests)))

(use-package web-mode
    :config
    (setq web-mode-style-padding 0)
	(setq web-mode-css-indent-offset 0)
    (setq web-mode-code-indent-offset 0)
    (setq web-mode-markup-indent-offset 2)
    :mode
    (("\\.vue\\'" . web-mode)))

(use-package lsp-mode
  :config
  (setq lsp-inlay-hint-enable t)
  :hook
  (rust-mode . lsp-deferred)
  (go-ts-mode . lsp-deferred)
  (go-mode . lsp-deferred)
  (typescript-ts-mode . lsp-deferred)
  (js-ts-mode . lsp-deferred)
  (web-mode . lsp-deferred)
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
 '(custom-safe-themes
      '("dd4582661a1c6b865a33b89312c97a13a3885dc95992e2e5fc57456b4c545176"
           "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33"
           "e8bd9bbf6506afca133125b0be48b1f033b1c8647c628652ab7a2fe065c10ef0"
           "77fff78cc13a2ff41ad0a8ba2f09e8efd3c7e16be20725606c095f9a19c24d3d"
           "0c83e0b50946e39e237769ad368a08f2cd1c854ccbcd1a01d39fdce4d6f86478"
           "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
           "13096a9a6e75c7330c1bc500f30a8f4407bd618431c94aeab55c9855731a95e1"
           "7de64ff2bb2f94d7679a7e9019e23c3bf1a6a04ba54341c36e7cf2d2e56e2bcc"
           "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f"
           "b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22"
           default))
 '(package-selected-packages nil)
 '(safe-local-variable-values
      '((web-mode-indent-style . 2) (web-mode-block-padding . 2)
           (web-mode-script-padding . 2) (web-mode-style-padding . 2))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
