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

(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(use-package emacs
  :bind (([remap list-buffers] . ibuffer)
	 ("M-s-<left>" . previous-buffer)
	 ("M-s-<right>" . next-buffer)
	 ("M-<f7>" . xref-find-referencess)
     ("s-/" . comment-line)
     ("s-w" . er/expand-region)
     ("s-d" . duplicate-thing)       
	 ("s-<backspace>" . kill-whole-line)
	 ("C-S-j" . (lambda() (interactive)
                    (next-line)
                    (join-line)))
	 ("s-<return>" . (lambda() (interactive)
			   (move-end-of-line nil)
			   (electric-newline-and-maybe-indent)))
	 ("<f5>" . deadgrep)
	 ("S-<f6>" . lsp-rename)
	 ("s-b" . xref-find-definitions)
	 ("s-f" . consult-ripgrep)
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
  (setq backup-directory-alist '(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
  (setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))
  (setq create-lockfiles nil)  
    )


(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package duplicate-thing)

(use-package expand-region)

(use-package posframe)

(use-package vertico-posframe
  :init
  (vertico-posframe-mode 1))


;; Remove the border
(setq modus-themes-common-palette-overrides
      '((border-mode-line-active unspecified)
	(border-mode-line-inactive unspecified)))

(load-theme 'modus-operandi-tinted)

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
    (before-save-hook . gofmt-before-save))

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

(use-package gptel
    :config
    (setq
        gptel-model 'qwen3-coder:30b
        gptel-backend (gptel-make-ollama "Ollama"
                          :host "localhost:11434"
                          :stream t
                          :models '(qwen3-coder:30b))))

(use-package magit)

(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  ;; Add Embark to the mouse context menu. Also enable `context-menu-mode'.
  ;; (context-menu-mode 1)
  ;; (add-hook 'context-menu-functions #'embark-context-menu 100)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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
